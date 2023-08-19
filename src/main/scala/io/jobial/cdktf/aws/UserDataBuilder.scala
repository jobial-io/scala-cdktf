package io.jobial.cdktf.aws

import cats.Traverse.ops.toAllTraverseOps
import cats.data.State
import cats.effect.Concurrent
import cats.effect.IO
import cats.effect.Timer
import cats.implicits.catsStdInstancesForList
import cats.implicits.catsSyntaxFlatMapOps
import io.jobial.scase.aws.client.S3Client
import io.jobial.sprint.process.ProcessContext
import io.jobial.sprint.util.CatsUtils
import org.apache.commons.io.IOUtils

import java.io.File
import java.io.FileInputStream
import java.nio.file.Paths
import java.util.UUID.randomUUID
import scala.sys.props

trait UserDataBuilder extends CatsUtils[IO] with S3Client[IO] {

  type UserData = State[IO[String], IO[String]]

  def evaluate(data: UserData) =
    data.run(pure("")).map(_._1).value

  def userData(data: UserData) =
    evaluate(shebang >> data)

  def addUserData(d: IO[String]): UserData =
    State.inspect { _: IO[String] =>
      d
    }.modify { data =>
      for {
        data <- data
        r <- d
      } yield {
        //println("+" + r)
        data + r
      }
    }

  implicit def addUserData(data: String): UserData =
    addUserData(pure(data))

  def addUserDataLines(data: String): UserData =
    addUserData(s"$data\n")

  def addFile(path: String, content: IO[String], overwrite: Boolean = true): UserData = addUserData {
    for {
      content <- content
    } yield {
      s"""
mkdir -p ${Option(Paths.get(path).getParent).getOrElse("/")}
cat <<EOF ${if (overwrite) ">" else ">>"}${path}
${content}EOF
"""
    }
  }

  def addFile(path: String, content: String): UserData =
    addFile(path, pure(content))

  def addFile(path: String, data: UserData): UserData =
    addFile(path, evaluate(data))

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
yum -y install docker criu
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

  def addUserSshPublicKey(overwrite: Boolean = true) =
    addFileFromHome(".ssh/id_rsa.pub") >>
      addUserDataLines(s"cat /home/ec2-user/.ssh/id_rsa.pub ${if (overwrite) ">" else ">>"} /home/ec2-user/.ssh/authorized_keys") >>
      addUserDataLines("chown ec2-user /home/ec2-user/.ssh/id_rsa.pub")
  
  def ecrLogin(region: String, accountId: String) = addUserDataLines(
    s"aws ecr get-login-password --region $region | docker login --username AWS --password-stdin $accountId.dkr.ecr.$region.amazonaws.com"
  )

  def docker(args: String*) = addUserDataLines(
    ("docker" :: args.toList).mkString(" ")
  )

  def addRoute53Record(name: String, hostedZone: String, description: String, ttl: Int = 30) = addUserData {
    """
# create or update the given alias record and associate with the address of this instance
function update_alias_record() {
    hosted_zone_name="$1"
    record_name="$2"
    comment="$3"
    
    address="$(curl http://169.254.169.254/latest/meta-data/local-ipv4)"

    hosted_zone_id=$(aws route53 list-hosted-zones --query 'HostedZones[?Name==`'"$hosted_zone_name"'.`].Id' --output text)
    
    aws route53 change-resource-record-sets --hosted-zone-id "$hosted_zone_id" \
    --change-batch '{ "Comment": "'"$comment"'","Changes": [ { "Action": "UPSERT", "ResourceRecordSet": { "Name": "'"$record_name"'", "Type": "A", "TTL": """ + ttl + """, "ResourceRecords": [ { "Value": "'"$address"'" } ] } } ] }'
}
""" +
      s"""
update_alias_record $hostedZone $name.$hostedZone "$description"
"""
  }

  def yumInstall(packages: String*) =
    addUserDataLines(s"yum install -y ${packages.mkString(" ")}")

  def addCrontab(cronLines: List[(String, UserData)]): UserData =
    for {
      _ <- addFile("/tmp/crontab", cronLines.map(l => addUserData(s"${l._1}\t\t") >> s"${l._2}\n").reduce(_ >> _))
      _ <- addUserDataLines("crontab /tmp/crontab ; rm -f /tmp/crontab")
      r <- addUserDataLines("rm -f /var/run/crond.reboot ; systemctl restart crond")
    } yield r

  def addCrontab(cronLines: (String, UserData)*): UserData =
    addCrontab(cronLines.toList)

  def copyPath(fromPath: String, toPath: String, bucket: String, bucketPath: String)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]): UserData =
    addUserData {
      val bucketUri = s"s3://$bucket$bucketPath"
      s3Sync(fromPath, bucketUri) >>
        pure(s"""aws s3 sync ${bucketUri} ${toPath}""")
    }

  def copyFromHome(path: String, bucket: String = "cbtech", bucketPath: String = "tmp")(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]): UserData =
    copyPath(s"${sys.props("user.home")}/$path", s"/home/ec2user/$path", bucket, s"$bucketPath/${randomUUID}/")

  def addDockerCheckpointOnShutdown(developerEnvName: String) = {
    val checkpointContainer = "/usr/local/bin/checkpoint_container"
    addFile(checkpointContainer, pure(
      s"""#!/bin/bash
docker checkpoint create developer-env-${developerEnvName} \\$$(date +%Y%m%d-%H%M%S) 
""")
    ) >>
      addUserDataLines(s"chmod +x ${checkpointContainer}") >>
      addFile("/etc/systemd/system/docker.service.d/override.conf", pure(
        s"""[Service]
ExecStop=
ExecStop=${checkpointContainer}
""")
      ) >>
      addUserDataLines("systemctl daemon-reload")
  }

  val installPodman = addUserDataLines("""
yum install -y yajl docker criu
rpm -ivh --force https://rpmfind.net/linux/fedora/linux/updates/38/Everything/x86_64/Packages/p/podman-4.6.1-1.fc38.x86_64.rpm \
  https://rpmfind.net/linux/fedora/linux/releases/38/Everything/x86_64/os/Packages/c/catatonit-0.1.7-14.fc38.x86_64.rpm \
  http://www.rpmfind.net/linux/fedora/linux/updates/37/Everything/x86_64/Packages/c/conmon-2.1.7-2.fc37.x86_64.rpm \
  https://rpmfind.net/linux/fedora/linux/updates/38/Everything/x86_64/Packages/c/containers-common-extra-1-89.fc38.noarch.rpm \
  https://dl.fedoraproject.org/pub/fedora/linux/releases/38/Everything/x86_64/os/Packages/s/shadow-utils-subid-4.13-6.fc38.x86_64.rpm \
  https://rpmfind.net/linux/fedora/linux/updates/38/Everything/x86_64/Packages/n/netavark-1.7.0-1.fc38.x86_64.rpm \
  https://rpmfind.net/linux/fedora/linux/updates/38/Everything/x86_64/Packages/c/containers-common-1-89.fc38.noarch.rpm \
  https://rpmfind.net/linux/fedora/linux/releases/38/Everything/x86_64/os/Packages/c/crun-1.8.3-2.fc38.x86_64.rpm \
  http://www.rpmfind.net/linux/fedora/linux/releases/38/Everything/x86_64/os/Packages/s/slirp4netns-1.2.0-12.fc38.x86_64.rpm \
  http://www.rpmfind.net/linux/fedora/linux/releases/38/Everything/x86_64/os/Packages/l/libslirp-4.7.0-3.fc38.x86_64.rpm \
  http://www.rpmfind.net/linux/fedora/linux/releases/38/Everything/x86_64/os/Packages/g/glib-1.2.10-68.fc38.x86_64.rpm \
  https://dl.fedoraproject.org/pub/fedora/linux/releases/38/Everything/x86_64/os/Packages/c/criu-libs-3.17.1-5.fc38.x86_64.rpm \
  https://rpmfind.net/linux/fedora/linux/releases/38/Everything/x86_64/os/Packages/c/criu-3.17.1-5.fc38.x86_64.rpm \
  https://rpmfind.net/linux/fedora/linux/releases/38/Everything/x86_64/os/Packages/n/nftables-1.0.5-2.fc38.x86_64.rpm

yum rm -y docker
""")

  def podmanEcrLogin(region: String, accountId: String) = addUserDataLines(
    s"aws ecr get-login-password --region $region | podman login --username AWS --password-stdin $accountId.dkr.ecr.$region.amazonaws.com"
  )

  def addPodmanCheckpointOnShutdown(name: String) = {
    val checkpointContainer = s"/usr/local/bin/podman_checkpoint_${name}"
    addFile(checkpointContainer, pure(
      s"""#!/bin/bash
podman container checkpoint ${name} 
""")
    ) >>
      addUserDataLines(s"chmod +x ${checkpointContainer}") >>
      addFile("/etc/systemd/system/podman.service.d/override.conf", pure(
        s"""[Service]
ExecStop=
ExecStop=${checkpointContainer}
""")
      ) >>
      addUserDataLines("systemctl daemon-reload")
  }

  def podman(args: String*) = addUserDataLines(
    ("podman" :: args.toList).mkString(" ")
  )

  def podmanRestoreOrRun(name: String, args: String*) = addUserDataLines(
    s"podman container restore ${name} || podman run --name ${name} ${args.toList.mkString(" ")}"
  )

}
