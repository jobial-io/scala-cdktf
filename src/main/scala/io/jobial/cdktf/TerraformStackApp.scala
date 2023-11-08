package io.jobial.cdktf

import cats.effect.IO
import io.jobial.cdktf.aws.TerraformStackBuildContext
import io.jobial.sclap.CommandLineApp
import io.jobial.sprint.process.ProcessContext
import io.jobial.sprint.process.ProcessManagement

trait TerraformStackApp[D] extends CommandLineApp with ProcessManagement[IO] {

  def run(stack: IO[TerraformStackBuildContext[D]]) =
    command.printStackTraceOnException(true) {
      for {
        subcommands <- subcommands(
          runDeploy(stack),
          runDestroy(stack),
          runRedeploy(stack),
          runPlan(stack)
        )
      } yield subcommands orElse runStack(stack)
    }

  def runStack(stack: IO[TerraformStackBuildContext[D]]) =
    for {
      stack <- stack
      r <- stack.synth
    } yield stack

  def terraformContext(context: TerraformStackBuildContext[D]) = ProcessContext(
    directory = Some(s"${context.app.getOutdir}/stacks/${context.name}"),
    inheritIO = true
  )

  def terraform(stack: TerraformStackBuildContext[D], args: String*)(implicit processContext: ProcessContext) =
    runProcessAndWait("terraform" :: args.toList)

  def runTerraformCommand(stack: TerraformStackBuildContext[D])(f: ProcessContext => IO[Any]) =
    f(terraformContext(stack))

  lazy val autoApproveOpt = opt[Boolean]("auto-approve")
    .default(true)
    .description("Auto-approve terraform actions")

  def runDeploy(stack: IO[TerraformStackBuildContext[D]], description: Option[String] = None, destroyFirst: Boolean = false) =
    subcommand("deploy")
      .description(description.getOrElse("Deploy terraform stack")) {
        runDeployCommandLine(stack, destroyFirst)
      }

  def runDeployCommandLine(stack: IO[TerraformStackBuildContext[D]], destroyFirst: Boolean) =
    for {
      autoApprove <- autoApproveOpt
    } yield deploy(stack, autoApprove, destroyFirst)

  def deploy(stack: IO[TerraformStackBuildContext[D]], autoApprove: Boolean, destroyFirst: Boolean): IO[Any] =
    for {
      stack <- runStack(stack)
      r <- deploy(stack, autoApprove, destroyFirst)
    } yield r

  def deploy(stack: TerraformStackBuildContext[D], autoApprove: Boolean, destroyFirst: Boolean): IO[Any] =
    for {
      _ <- whenA(destroyFirst)(destroy(pure(stack), autoApprove))
      _ <- beforeDeploy(stack)
      r <- runTerraformCommand(stack) { implicit processContext =>
        val args = "apply" :: (if (autoApprove) List("-auto-approve") else List())
        (
          printLn(s"Deploying ${stack.name}") >>
            terraform(stack, "init") >>
            terraform(stack, "plan") >>
            terraform(stack, args: _*)
          ).guarantee(afterDeploy(stack))
      }
    } yield r

  def beforeDeploy(context: TerraformStackBuildContext[D]) = unit

  def afterDeploy(context: TerraformStackBuildContext[D]) = unit

  def runDestroy(stack: IO[TerraformStackBuildContext[D]], description: Option[String] = None) =
    subcommand("destroy")
      .description(description.getOrElse("Destroy terraform stack")) {
        for {
          autoApprove <- autoApproveOpt
        } yield destroy(stack, autoApprove)
      }

  def destroy(stack: IO[TerraformStackBuildContext[D]], autoApprove: Boolean) =
    for {
      stack <- runStack(stack)
      _ <- beforeDestroy(stack)
      r <- runTerraformCommand(stack) { implicit processContext =>
        val args = "destroy" :: (if (autoApprove) List("-auto-approve") else List())
        (
          printLn(s"Destroying ${stack.name}") >>
            terraform(stack, "init") >>
            terraform(stack, "plan") >>
            terraform(stack, args: _*)
          ).guarantee(afterDestroy(stack))
      }
    } yield r

  def beforeDestroy(context: TerraformStackBuildContext[D]) = unit

  def afterDestroy(context: TerraformStackBuildContext[D]) = unit

  def runRedeploy(stack: IO[TerraformStackBuildContext[D]], description: Option[String] = None) =
    subcommand("redeploy")
      .description(description.getOrElse("Destroy and redeploy terraform stack")) {
        runDeployCommandLine(stack, true)
      }

  def runPlan(stack: IO[TerraformStackBuildContext[D]], description: Option[String] = None) =
    subcommand("plan")
      .description(description.getOrElse("Run terraform plan for the stack")) {
        for {
          stack <- runStack(stack)
          r <- runTerraformCommand(stack) { implicit processContext =>
            printLn(s"Planning ${stack.name}") >>
              terraform(stack, "plan")
          }
        } yield r
      }

}
