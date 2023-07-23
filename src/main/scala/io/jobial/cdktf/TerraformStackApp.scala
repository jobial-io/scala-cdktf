package io.jobial.cdktf

import cats.effect.IO
import cats.effect.IO.pure
import io.jobial.cdktf.aws.TerraformStackBuildContext
import io.jobial.sclap.CommandLineApp
import io.jobial.sprint.process.ProcessContext
import io.jobial.sprint.process.ProcessManagement

trait TerraformStackApp[D] extends CommandLineApp with ProcessManagement[IO] {

  def run =
    command.printStackTraceOnException(true) {
      for {
        deploy <- runDeploy
        plan <- runPlan
      } yield deploy orElse plan orElse runStack
    }

  def stack: IO[TerraformStackBuildContext[D]]

  def runStack =
    for {
      stack <- stack
      r <- stack.synth
    } yield r

  def terraformContext(context: TerraformStackBuildContext[D]) = ProcessContext(
    directory = Some(s"./cdktf.out/stacks/${context.name}"),
    inheritIO = true
  )

  def terraform(stack: TerraformStackBuildContext[D], args: String*)(implicit processContext: ProcessContext) =
    runProcessAndWait("terraform" +: args)

  def runTerraformCommand(stack: IO[TerraformStackBuildContext[D]])(f: (TerraformStackBuildContext[D], ProcessContext) => IO[Any]) =
    for {
      stack <- stack
      r <- f(stack, terraformContext(stack))
    } yield r

  def runDeploy = subcommand("deploy") {
    for {
      r <- runTerraformCommand(stack) { (stack, context) =>
        pure(println(s"Deploying ${stack.name}")) >>
          terraform(stack, "plan")(context) >>
          terraform(stack, "apply")(context)
      }
    } yield r
  }
  
  def runPlan = subcommand("plan") {
    for {
      r <- runTerraformCommand(stack) { (stack, context) =>
        pure(println(s"Planning ${stack.name}")) >>
          terraform(stack, "plan")(context)
      }
    } yield r
  }

}
