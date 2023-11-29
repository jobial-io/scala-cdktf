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

  def retry[T](delay: FiniteDuration = 2.seconds)(f: IO[T])(implicit concurrent: Concurrent[IO], timer: Timer[IO]): IO[T] =
    f.handleErrorWith { _ =>
      printStr(".") >>
        sleep(delay) >>
        retry(delay)(f)
    }

  def retryForHost[T](hostName: String, port: Int = 22, delay: FiniteDuration = 2.seconds)(f: IO[T])(implicit concurrent: Concurrent[IO], timer: Timer[IO]): IO[T] =
    retry(delay)(checkHostPort(hostName, port) >> f)

  def copyFileToHost(path: String, hostName: String, hostPath: String, delay: FiniteDuration = 2.seconds)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) =
    retryForHost(hostName, delay = delay) {
      implicit val processContext = ProcessContext(inputFilename = Some(expandHome(path)), inheritIO = false)
      runProcessAndWait(List("ssh", "-t", "-o", "StrictHostKeyChecking=no", s"ec2-user@${hostName}", "bash", "-c", s"'[ -e ${hostPath} ] || ( mkdir -p ${parentPath(hostPath)}; cat > ${hostPath}; chmod 600 ${hostPath} )'"))
    }

  def rsyncToHost(path: String, hostName: String, hostPath: String, options: List[String] = List(), delay: FiniteDuration = 2.seconds)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) =
    retryForHost(hostName, delay = delay) {
      runOnEc2Host(hostName, "mkdir", "-p", hostPath) >>
        runProcessWithTerminal(List("rsync", "-av") ++ options ++ List(path, s"ec2-user@$hostName:$hostPath"))
    }

  def runOnEc2Host(hostName: String, args: String*)(implicit processContext: ProcessContext, concurrent: Concurrent[IO], timer: Timer[IO]) =
    runProcessWithTerminal(List("ssh", "-t", "-o", "StrictHostKeyChecking=no", s"ec2-user@${hostName}") ++ args.toList)

}
