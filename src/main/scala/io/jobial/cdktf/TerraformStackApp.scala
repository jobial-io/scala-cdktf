package io.jobial.cdktf

import cats.effect.IO
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
    runProcessAndWait("terraform" +: args)

  def runTerraformCommand(stack: TerraformStackBuildContext[D])(f: ProcessContext => IO[Any]) =
    f(terraformContext(stack))

  val autoApproveOpt = opt[Boolean]("auto-approve")
    .default(true)
    .description("Auto-approve terraform actions")

  def runDeploy(stack: IO[TerraformStackBuildContext[D]], description: Option[String] = None) =
    subcommand("deploy")
      .description(description.getOrElse("Deploy terraform stack")) {
        for {
          autoApprove <- autoApproveOpt
        } yield deploy(stack, autoApprove)
      }

  def deploy(stack: IO[TerraformStackBuildContext[D]], autoApprove: Boolean) =
    for {
      stack <- runStack(stack)
      r <- runTerraformCommand(stack) { implicit processContext =>
        val args = "apply" :: (if (autoApprove) List("-auto-approve") else List())
        printLn(s"Deploying ${stack.name}") >>
          terraform(stack, "init") >>
          terraform(stack, "plan") >>
          terraform(stack, args: _*) >>
          afterDeploy(stack)
      }
    } yield r

  def afterDeploy(context: TerraformStackBuildContext[D]): IO[_] = unit

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
      r <- runTerraformCommand(stack) { implicit processContext =>
        val args = "destroy" :: (if (autoApprove) List("-auto-approve") else List())
        printLn(s"Destroying ${stack.name}") >>
          terraform(stack, "init") >>
          terraform(stack, "plan") >>
          terraform(stack, args: _*) >>
          afterDestroy(stack)
      }
    } yield r

  def afterDestroy(context: TerraformStackBuildContext[D]): IO[_] = unit

  def runRedeploy(stack: IO[TerraformStackBuildContext[D]]) =
    subcommand("redeploy")
      .description("Destroy and redeploy terraform stack") {
        for {
          autoApprove <- autoApproveOpt
        } yield destroy(stack, autoApprove) >> deploy(stack, autoApprove)
      }

  def runPlan(stack: IO[TerraformStackBuildContext[D]]) = subcommand("plan") {
    for {
      stack <- runStack(stack)
      r <- runTerraformCommand(stack) { implicit processContext =>
        printLn(s"Planning ${stack.name}") >>
          terraform(stack, "plan")
      }
    } yield r
  }

}
