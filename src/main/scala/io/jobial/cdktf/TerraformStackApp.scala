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
        deploy <- runDeploy(stack)
        destroy <- runDestroy(stack)
        plan <- runPlan(stack)
      } yield deploy orElse plan orElse runStack(stack)
    }

  def runStack(stack: IO[TerraformStackBuildContext[D]]) =
    for {
      stack <- stack
      r <- stack.synth
    } yield stack

  def terraformContext(context: TerraformStackBuildContext[D]) = ProcessContext(
    directory = Some(s"./cdktf.out/stacks/${context.name}"),
    inheritIO = true
  )

  def terraform(stack: TerraformStackBuildContext[D], args: String*)(implicit processContext: ProcessContext) =
    runProcessAndWait("terraform" +: args)

  def runTerraformCommand(stack: TerraformStackBuildContext[D])(f: ProcessContext => IO[Any]) =
    f(terraformContext(stack))

  val autoApproveOpt = opt[Boolean]("auto-approve").default(true)

  def runDeploy(stack: IO[TerraformStackBuildContext[D]]) = subcommand("deploy") {
    for {
      autoApprove <- autoApproveOpt
    } yield deploy(stack, autoApprove)
  }

  def deploy(stack: IO[TerraformStackBuildContext[D]], autoApprove: Boolean) =
    for {
      stack <- runStack(stack)
      r <- runTerraformCommand(stack) { implicit processContext =>
        val args = "apply" :: (if (autoApprove) List("-auto-approve") else List())
        pure(println(s"Deploying ${stack.name}")) >>
          terraform(stack, "plan") >>
          terraform(stack, args: _*)
      }
    } yield r

  def runDestroy(stack: IO[TerraformStackBuildContext[D]]) = subcommand("destroy") {
    for {
      autoApprove <- autoApproveOpt
    } yield destroy(stack, autoApprove)
  }

  def destroy(stack: IO[TerraformStackBuildContext[D]], autoApprove: Boolean) =
    for {
      stack <- runStack(stack)
      r <- runTerraformCommand(stack) { implicit processContext =>
        val args = "destroy" :: (if (autoApprove) List("-auto-approve") else List())
        pure(println(s"Destroying ${stack.name}")) >>
          terraform(stack, args: _*)
      }
    } yield r

  def runRedeploy(stack: IO[TerraformStackBuildContext[D]]) = subcommand("redeploy") {
    for {
      autoApprove <- autoApproveOpt
    } yield destroy(stack, autoApprove) >> deploy(stack, autoApprove)
  }

  def runPlan(stack: IO[TerraformStackBuildContext[D]]) = subcommand("plan") {
    for {
      stack <- runStack(stack)
      r <- runTerraformCommand(stack) { implicit processContext =>
        pure(println(s"Planning ${stack.name}")) >>
          terraform(stack, "plan")
      }
    } yield r
  }

}
