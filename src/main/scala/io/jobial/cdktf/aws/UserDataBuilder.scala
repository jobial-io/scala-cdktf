package io.jobial.cdktf.aws

import cats.data.State
import cats.effect.IO
import cats.implicits.catsSyntaxFlatMapOps
import io.jobial.sprint.util.CatsUtils
import org.apache.commons.io.IOUtils

import java.io.File
import java.io.FileInputStream
import scala.sys.props

trait UserDataBuilder extends CatsUtils[IO] {
  this: TerraformStackBuilder =>

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

  def addUserDataLines(data: String): UserDataState =
    addUserData(pure(s"\n$data\n"))

  def addFile(path: String, content: IO[String], overwrite: Boolean = true) = addUserData {
    for {
      content <- content
    } yield
      s"""
mkdir -p ${new File(path).getParentFile.getPath}
cat <<EOF ${if (overwrite) ">" else ">>"}${path}
${content}EOF
"""
  }

  def addToFile(path: String, content: String) =
    addFile(path, pure(content), false)

  def addFileFromHome(path: String, targetDir: String = "/home/ec2-user") =
    for {
      content <- readFileFromHome(path)
      _ <- addFile(s"$targetDir/${path}", content)
    } yield ()

  def readFile(path: String) = State.inspect { _: IO[String] =>
    delay(IOUtils.toString(new FileInputStream(path)))
  }

  def readFileFromHome(path: String) =
    readFile(s"${props("user.home")}/${path}")

  val shebang = addUserData("""#!/bin/bash
""")

  val installDocker = addUserData("""
yum -y install docker
usermod -a -G docker ec2-user
id ec2-user
newgrp docker
yum install python3-pip
pip3 install docker-compose
echo '{ "experimental": true }' > /etc/docker/daemon.json
systemctl enable docker.service
systemctl start docker.service
systemctl status docker.service
""")

  val addUserAwsCredentials =
    addFileFromHome(".aws/credentials") >>
      addFileFromHome(".aws/credentials", "/root")

  val addUserSshPublicKey =
    addFileFromHome(".ssh/id_rsa.pub") >>
      addUserDataLines("cat /home/ec2-user/.ssh/id_rsa.pub >> /home/ec2-user/.ssh/authorized_keys") >>
      addUserDataLines("chown ec2-user /home/ec2-user/.ssh/id_rsa.pub")

  val addUserSshPrivateKey =
    addFileFromHome(".ssh/id_rsa") >>
      addUserDataLines("chown ec2-user /home/ec2-user/.ssh/id_rsa") >>
      addUserDataLines("chmod 600 /home/ec2-user/.ssh/id_rsa")

  def ecrLogin(region: String, accountId: String) = addUserDataLines(
    s"aws ecr get-login-password --region $region | docker login --username AWS --password-stdin $accountId.dkr.ecr.$region.amazonaws.com"
  )

  def docker(args: String*) = addUserDataLines(
    ("docker" :: args.toList).mkString(" ")
  )

  def addRoute53Record(name: String, hostedZone: String, description: String) = addUserData {
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
