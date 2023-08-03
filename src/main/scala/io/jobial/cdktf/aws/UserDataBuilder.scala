package io.jobial.cdktf.aws

import cats.data.State
import cats.effect.IO
import cats.implicits.catsSyntaxFlatMapOps
import io.jobial.sprint.util.CatsUtils
import org.apache.commons.io.IOUtils

import java.io.FileInputStream
import scala.sys.props

trait UserDataBuilder extends CatsUtils[IO] {

  type UserDataState = State[IO[String], IO[String]]

  def userData(data: UserDataState) =
    (shebang >> data).run(pure("")).map(_._1).value

  def addUserData(d: IO[String]): UserDataState =
    State.inspect { _: IO[String] =>
      d
    }.modify { data =>
      for {
        data <- data
        r <- d
      } yield data + r
    }

  def addUserData(data: String): UserDataState =
    addUserData(pure(data))

  val shebang = addUserData("""#!/bin/bash
""")

  val installDocker = addUserData("""
yum -y install docker
usermod -a -G docker ec2-user
id ec2-user
newgrp docker
yum install python3-pip
pip3 install docker-compose
systemctl enable docker.service
systemctl start docker.service
systemctl status docker.service
""")

  val addUserAwsCredentials = addUserData {
    for {
      credentials <- delay(IOUtils.toString(new FileInputStream(s"${props("user.home")}/.aws/credentials")))
    } yield
      s"""
mkdir /root/.aws
cat <<EOF >/root/.aws/credentials
${credentials}EOF
"""
  }

  val addUserSshPublicKey = addUserData {
    for {
      sshPublicKey <- delay(IOUtils.toString(new FileInputStream(s"${props("user.home")}/.ssh/id_rsa.pub")))
    } yield
      s"""
cat <<EOF >>/home/ec2-user/.ssh/authorized_keys
${sshPublicKey}EOF
"""
  }

  def ecrLogin(region: String, accountId: String) = addUserData(
    s"\naws ecr get-login-password --region $region | docker login --username AWS --password-stdin $accountId.dkr.ecr.$region.amazonaws.com"
  )

  def docker(args: String*) = addUserData(
    ("\ndocker" :: args.toList).mkString(" ")
  )

  def addRoute53Record(hostedZone: String, name: String, description: String) = addUserData {
    """
# create or update the given alias record and associate with the address of this instance
function update_alias_record() {
    hosted_zone_name="$1"
    record_name="$2"
    comment="$3"
    
    address="$(curl http://169.254.169.254/latest/meta-data/local-ipv4)"

    hosted_zone_id=$(aws route53 list-hosted-zones --query 'HostedZones[?Name==`'"$hosted_zone_name"'.`].Id' --output text)
    
    aws route53 change-resource-record-sets --hosted-zone-id "$hosted_zone_id" \
    --change-batch '{ "Comment": "'"$comment"'","Changes": [ { "Action": "UPSERT", "ResourceRecordSet": { "Name": "'"$record_name"'", "Type": "A", "TTL": 120, "ResourceRecords": [ { "Value": "'"$address"'" } ] } } ] }'
}
""" +
      s"""
update_alias_record $hostedZone $name.$hostedZone "$description"
"""
  }
}
