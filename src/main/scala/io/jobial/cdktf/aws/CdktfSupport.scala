package io.jobial.cdktf.aws

import cats.effect.IO
import cats.implicits.catsKernelStdOrderForString
import cats.implicits.catsSyntaxEq
import cats.implicits.catsSyntaxFlatMapOps
import io.jobial.sprint.process.ProcessManagement
import io.jobial.sprint.util.CatsUtils
import org.apache.commons.io.IOUtils.write
import java.nio.file.Files
import java.io.File
import java.io.FileOutputStream
import java.nio.file.Files
import java.util.UUID.randomUUID

trait CdktfSupport extends CatsUtils[IO] with ProcessManagement[IO] {

  def setWorkingDirectory(stackName: String) =
    for {
      sourceLocation <- pure(getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath)
      _ <- printLn(s"Source location is $sourceLocation")
      workingDirectory = if (new File(sourceLocation).exists && new File(sourceLocation).isDirectory) sourceLocation.replaceAll("/target/classes/$", "") else s"${sys.props("java.io.tmpdir")}/$stackName"
      _ <- delay(new File(workingDirectory).mkdirs)
      _ <- delay(System.setProperty("user.dir", s"$workingDirectory/"))
      _ <- printLn(s"Working directory is set to $workingDirectory")
    } yield workingDirectory

  def workingDirectory = delay(new File(sys.props("user.dir")).getCanonicalPath)

  def generateCdktfConfig(stackName: String, projectId: String = randomUUID.toString, app: String = "mvn -e -q compile exec:java") = {
    for {
      _ <- setWorkingDirectory(stackName)
      workingDirectory <- workingDirectory
      config <- pure(s"""{
  "language": "java",
  "app": "${app}",
  "projectId": "${projectId}",
  "sendCrashReports": "true",
  "codeMakerOutput": "src/main/java/imports",
  "terraformProviders": [],
  "terraformModules": [],
  "context": {
    "excludeStackIdFromLogicalIds": "true",
    "allowSepCharsInLogicalIds": "true"
  }
}
""")
      _ <- if (new File(s"${workingDirectory}/cdktf.json").exists)
        printLn(s"Config ${workingDirectory}/cdktf.json already exists")
      else
        printLn(s"Generating ${workingDirectory}/cdktf.json") >>
          delay(write(config, new FileOutputStream(s"${workingDirectory}/cdktf.json"), "utf-8")) >>
          delay(new File(s"${workingDirectory}/cdktf.out/stacks/${stackName}").mkdirs())

    } yield config
  }

}
