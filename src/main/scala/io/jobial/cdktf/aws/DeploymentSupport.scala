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
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.sys.props

trait DeploymentSupport extends CatsUtils[IO] with ProcessManagement[IO] {

  def parentPath(path: String) =
    Option(Paths.get(path).getParent).getOrElse("/")

  def fileExists(path: String) =
    new File(expandHome(path)).exists

  def expandHome(path: String) =
    if (path.startsWith("~/")) props("user.home") + "/" + path.substring(2)
    else path

  def checkHostPort(hostName: String, port: Int)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) = {
    implicit val processContext = ProcessContext(inheritIO = false)
    runProcessAndWait(List("nc", "-zv", hostName, port.toString))
  }

  def retryForHost[T](hostName: String, port: Int = 22, delay: FiniteDuration = 2.seconds)(f: IO[T])(implicit concurrent: Concurrent[IO], timer: Timer[IO]): IO[T] = {
    checkHostPort(hostName, port) >> f
  }.handleErrorWith { _ =>
    printStr(".") >>
      sleep(delay) >>
      retryForHost(hostName, port, delay)(f)
  }

  def copyFileToHost(path: String, hostName: String, hostPath: String, delay: FiniteDuration = 2.seconds)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) =
    retryForHost(hostName) {
      implicit val processContext = ProcessContext(inputFilename = Some(expandHome(path)), inheritIO = false)
      runProcessAndWait(List("ssh", "-t", s"ec2-user@${hostName}", "bash", "-c", s"'[ -e ${hostPath} ] || ( mkdir -p ${parentPath(hostPath)}; cat > ${hostPath}; chmod 600 ${hostPath} )'"))
    }

  def rsyncToHost(path: String, hostName: String, hostPath: String, delay: FiniteDuration = 2.seconds)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) =
    retryForHost(hostName) {
      runOnEc2Host("ssh", s"ec2-user@$hostName", "mkdir", "-p", hostPath) >>
        runProcessWithTerminal("rsync", "-av", path, s"ec2-user@$hostName:$hostPath")
    }

  def runOnEc2Host(hostName: String, args: String*)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) =
    runProcessWithTerminal(List("ssh", "-t", s"ec2-user@${hostName}") ++ args.toList)
  
}
