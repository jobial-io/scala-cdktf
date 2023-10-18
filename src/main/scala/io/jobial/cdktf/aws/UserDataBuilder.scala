package io.jobial.cdktf.aws

import cats.data.State
import cats.effect.Concurrent
import cats.effect.IO
import cats.effect.Timer
import cats.implicits.catsSyntaxFlatMapOps
import io.jobial.scase.aws.client.S3Client
import io.jobial.sprint.process.ProcessContext
import io.jobial.sprint.util.CatsUtils
import org.apache.commons.io.IOUtils

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

  implicit def addUserData(d: IO[String]): UserData =
    State.inspect { _: IO[String] =>
      d
    }.modify { data =>
      for {
        data <- data
        r <- d
      } yield
        data + r
    }

  implicit def addUserData(data: String): UserData =
    addUserData(pure(data))

  def modify(data: UserData)(f: String => String) =
    data.modify { data =>
      for {
        data <- data
      } yield f(data)
    }

  def withNewLine(data: UserData) =
    modify(data) { data =>
      if (data.endsWith("\n"))
        data
      else
        s"${data}\n"
    }

  def addUserDataLines(data: String): UserData =
    withNewLine(data)

  def addFile(path: String, content: UserData, overwrite: Boolean = true, newLineAtEnd: Boolean = true): UserData =
    addUserDataLines(s"\nmkdir -p ${Option(Paths.get(path).getParent).getOrElse("/")}") >>
      addUserDataLines(s"cat <<EOF ${if (overwrite) ">" else ">>"}${path}\n") >>
      content >>
      (if (newLineAtEnd) "\n" else "") >>
      "EOF\n\n"

  def addScript(path: String, content: UserData) =
    addFile(path, content) >>
      chmod("a+x", path)

  def addBashScript(path: String, content: UserData) =
    addScript(path, shebang >> content)

  def chmod(args: String*) =
    command("chmod", args)

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

  def shebang = addUserData("#!/bin/bash\n\n")

  def installDocker = addUserData("""
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

  def addUserAwsCredentials =
    addFileFromHome(".aws/credentials") >>
      addFileFromHome(".aws/credentials", "/root")

  def addUserSshPublicKey(overwrite: Boolean) =
    addFileFromHome(".ssh/id_rsa.pub") >>
      addUserDataLines(s"cat /home/ec2-user/.ssh/id_rsa.pub ${if (overwrite) ">" else ">>"} /home/ec2-user/.ssh/authorized_keys") >>
      addUserDataLines("chown ec2-user /home/ec2-user/.ssh/id_rsa.pub")

  def addUserSshPublicKey: UserData =
    addUserSshPublicKey(true)

  def ecrLogin(region: String, accountId: String) = addUserDataLines(
    s"aws ecr get-login-password --region $region | docker login --username AWS --password-stdin $accountId.dkr.ecr.$region.amazonaws.com"
  )

  def command(name: String, args: Seq[String]) = addUserDataLines(
    (name +: args).mkString(" ")
  )

  def docker(args: String*) =
    command("docker", args.toList)

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

  def yum(args: String*) =
    command("yum", args)

  def yumInstall(packages: String*) =
    yum(List("install", "-y") ++ packages: _*)

  def addCrontab(cronLines: List[(String, UserData)]): UserData =
    addFile("/tmp/crontab",
      addUserDataLines("PATH=$PATH:/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin") >>
        cronLines.map(l => addUserData(s"${l._1}\t\t") >> withNewLine(l._2)).reduce(_ >> _) >> "\n"
    ) >>
      addUserDataLines("crontab /tmp/crontab ; rm -f /tmp/crontab") >>
      addUserDataLines("rm -f /var/run/crond.reboot ; systemctl restart crond")

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
    addBashScript(checkpointContainer, s"""docker checkpoint create developer-env-${developerEnvName} \\$$(date +%Y%m%d-%H%M%S)""") >>
      addFile("/etc/systemd/system/docker.service.d/override.conf", pure(
        s"""[Service]
ExecStop=
ExecStop=${checkpointContainer}
""")
      ) >>
      addUserDataLines("systemctl daemon-reload")
  }

  def installPodman = addUserDataLines("""
yum install -y yajl docker criu yum-utils
yum-config-manager --add-repo https://rpmfind.net/linux/fedora/linux/updates/37/Everything/x86_64/
yum-config-manager --add-repo https://rpmfind.net/linux/fedora/linux/updates/38/Everything/x86_64/

yum install -y --nogpgcheck podman
yum rm -y docker

systemctl daemon-reload
systemctl enable podman
""")

  def podmanEcrLogin(region: String, accountId: String) = addUserDataLines(
    s"aws ecr get-login-password --region $region | podman login --username AWS --password-stdin $accountId.dkr.ecr.$region.amazonaws.com"
  )

  def addPodmanCheckpointOnShutdown(name: String) = {
    val checkpointContainer = s"/usr/local/bin/podman_checkpoint_${name}"
    addBashScript(checkpointContainer, s"""podman container checkpoint ${name} >>/var/log/podman_checkpoint_${name}.log 2>&1""") >>
      addFile("/etc/systemd/system/podman.service.d/override.conf", pure(
        s"""[Service]
ExecStop=
ExecStop=${checkpointContainer}
""")
      ) >>
      addUserDataLines("systemctl daemon-reload")
  }

  def podman(args: String*) =
    command("podman", args.toList)

  def aws(args: String*) =
    command("aws", args.toList)

  def podmanRestoreOrRun(name: String, args: String*) = addUserDataLines(
    s"podman container restore ${name} || podman run --name ${name} ${args.toList.mkString(" ")}"
  )

  def enableDBus = {
    val command = "export XDG_RUNTIME_DIR=/run/user/$(id -u)"
    addUserDataLines(command) >>
    addUserDataLines(s"""export DBUS_SESSION_BUS_ADDRESS="unix:path=$$XDG_RUNTIME_DIR/bus"""") >>
      addUserDataLines(s"echo '${command}' >>/etc/bashrc")
  }

  def addPodmanContainerToSystemd(name: String, user: String = "root") =
    addBashScript(s"/usr/local/bin/podman_checkpoint_${name}", s"""

podman container checkpoint ${name} -e /root/${name}.tar.gz && { 
  echo successful checkpoint
  podman stop ${name}
  podman rm ${name}
}
""") >>
      addBashScript(s"/usr/local/bin/podman_restore_${name}", s"""
status=$$(podman inspect --format="{{.State.Status}}" ${name})
[ "\\$$status" == running ] || ( podman container restore -i /root/${name}.tar.gz && echo successful restore ) || ( podman start ${name} && echo restore unsuccessful, starting container )
"""
      ) >>
      addUserDataLines(s"""container_id=$$(podman inspect --format="{{.Id}}" ${name})""") >>
      addFile(s"${homeDir(user)}/.config/systemd/user/${name}.service", s"""
[Unit]
Description=Service for podman container ${name} generated by scala-cdktf
Wants=network-online.target
After=network-online.target
RequiresMountsFor=/run/containers/storage

[Service]
Environment=PODMAN_SYSTEMD_UNIT=%n
Restart=on-failure
TimeoutStopSec=70
ExecStart=/usr/local/bin/podman_restore_${name}
ExecStop=/usr/local/bin/podman_checkpoint_${name}
ExecStopPost=/usr/local/bin/podman_checkpoint_${name}
PIDFile=/run/containers/storage/overlay-containers/$$container_id/userdata/conmon.pid
Type=forking
StandardOutput=append:/var/log/${name}.log
StandardError=append:/var/log/${name}.log

[Install]
WantedBy=default.target
"""
      ) >>
      enableDBus >>
      addUserDataLines(s"loginctl enable-linger ${user}") >>
      addUserDataLines(s"systemctl start user@0.service") >>
      addUserDataLines(s"systemctl daemon-reload") >>
      addUserDataLines(s"systemctl status systemd-logind") >>
      addUserDataLines(s"systemctl status dbus.socket") >>
      addUserDataLines(s"systemctl start dbus.socket") >>
      addUserDataLines(s"systemctl status dbus") >>
      addUserDataLines(s"systemctl start dbus") >>
      addUserDataLines(s"ls -al /run/user/") >>
      addUserDataLines(s"systemctl --user enable ${name}") >>
      addUserDataLines(s"systemctl --user daemon-reload") >>
      addUserDataLines(s"systemctl --user start ${name}")

  def homeDir(user: String) =
    user match {
      case "root" =>
        "/root"
      case _ =>
        s"/home/${user}"
    }
    
  def dockerPrune(limitInMb: Int = 10000) =
    addUserDataLines(s"[ $$(df -m --output=avail / | tail -n 1) -gt ${limitInMb} ] || (docker container prune ; docker image prune)")
}
