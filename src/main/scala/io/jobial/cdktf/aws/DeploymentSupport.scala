package io.jobial.cdktf.aws

import cats.effect.Concurrent
import cats.effect.IO
import cats.effect.Timer
import cats.implicits.catsSyntaxFlatMapOps
import io.jobial.sprint.process.ProcessContext
import io.jobial.sprint.process.ProcessManagement
import io.jobial.sprint.util.CatsUtils

import java.io.File
import java.nio.file.Paths
import scala.sys.props

trait DeploymentSupport extends CatsUtils[IO] with ProcessManagement[IO] {

  def parentPath(path: String) =
    Option(Paths.get(path).getParent).getOrElse("/")

  def fileExists(path: String) =
    new File(expandHome(path)).exists

  def expandHome(path: String) =
    if (path.startsWith("~/")) props("user.home") + "/" + path.substring(2)
    else path

  def checkHostPort(hostName: String)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) = {
    implicit val processContext = ProcessContext(inheritIO = false)
    runProcessAndWait(List("nc", "-zv", hostName, "22"))
  }

  def copyFileToHost(path: String, hostName: String, hostPath: String)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]): IO[_] = {
    checkHostPort(hostName) >> {
      implicit val processContext = ProcessContext(inputFilename = Some(expandHome(path)), inheritIO = false)
      runProcessAndWait(List("ssh", "-t", s"ec2-user@${hostName}", "bash", "-c", s"'[ -e ${hostPath} ] || ( mkdir -p ${parentPath(hostPath)}; cat > ${hostPath}; chmod 600 ${hostPath} )'"))
    }
  }.handleErrorWith { _ =>
    delay(print(".")) >>
      copyFileToHost(path, hostName, hostPath)
  }

  def rsyncToHost(path: String, hostName: String, hostPath: String)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]): IO[_] = {
    checkHostPort(hostName) >> {
      implicit val processContext = ProcessContext(inheritIO = true)
      runProcessAndWait(List("ssh", s"ec2-user@$hostName", "mkdir", "-p", hostPath)) >>
        runProcessAndWait(List("rsync", "-av", path, s"ec2-user@$hostName:$hostPath"))
    }
  }.handleErrorWith { _ =>
    delay(print(".")) >>
      rsyncToHost(path, hostName, hostPath)
  }

}
