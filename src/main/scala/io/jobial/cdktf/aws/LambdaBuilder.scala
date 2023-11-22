package io.jobial.cdktf.aws


import com.hashicorp.cdktf.providers.aws.cloudwatch_event_rule.CloudwatchEventRule
import com.hashicorp.cdktf.providers.aws.cloudwatch_event_target.CloudwatchEventTarget
import com.hashicorp.cdktf.providers.aws.iam_policy.IamPolicy
import com.hashicorp.cdktf.providers.aws.iam_role.IamRole
import com.hashicorp.cdktf.providers.aws.lambda_function.LambdaFunction
import com.hashicorp.cdktf.providers.aws.lambda_function.LambdaFunctionImageConfig
import com.hashicorp.cdktf.providers.aws.lambda_permission.LambdaPermission
import io.circe.Json

import java.util.UUID.randomUUID
import scala.collection.JavaConverters._

trait LambdaBuilder extends IamBuilder {

  def addLambdaFunction[D](
    name: String,
    imageUri: String,
    command: List[String],
    memorySize: Int = 128,
    timeout: Int = 3,
    role: IamRole,
    sourceCodeHash: String = randomUUID.toString,
    tags: Map[String, String] = Map()
  ): TerraformStackBuildState[D, LambdaFunction] = buildAndAddResource { context =>
    LambdaFunction.Builder
      .create(context.stack, s"$name-lambda-function")
      .functionName(name)
      .packageType("Image")
      .imageUri(imageUri)
      .imageConfig(LambdaFunctionImageConfig
        .builder
        .command(command.asJava)
        .build()
      )
      .timeout(timeout)
      .memorySize(memorySize)
      .role(role.getArn)
      .sourceCodeHash(sourceCodeHash)
      .tags(context.mergeTags(name, tags).asJava)
  }

  def addLambdaRoleWithStatements[D](
    name: String,
    statements: List[Json]
  ) = for {
    policy <- addPolicy[D](
      name,
      policy(statements)
    )
    logPolicy <- addPolicy[D](
      s"$name-log",
      DefaultLogPolicy
    )
    role <- addLambdaRole(
      name,
      List(policy, logPolicy)
    )
  } yield role

  def addLambdaRole[D](
    name: String,
    policies: List[IamPolicy]
  ) =
    addRole[D](
      s"$name-lambda",
      assumeRolePolicy = Some(DefaultLambdaAssumeRolePolicy),
      managedPolicyArns = policies.map(_.getArn)
    )

  def addCloudwatchEventRule[D](
    name: String,
    scheduleExpression: String
  ): TerraformStackBuildState[D, CloudwatchEventRule] = buildAndAddResource { context =>
    CloudwatchEventRule.Builder
      .create(context.stack, s"$name-cloudwatch-event-rule")
      .name(s"$name-cloudwatch-event-rule")
      .scheduleExpression(scheduleExpression)
  }

  def addCloudwatchEventTarget[D](
    name: String,
    lambda: LambdaFunction,
    rule: CloudwatchEventRule,
    input: Option[Json] = None
  ): TerraformStackBuildState[D, CloudwatchEventTarget] = buildAndAddResource { context =>
    CloudwatchEventTarget.Builder
      .create(context.stack, s"$name-cloudwatch-event-target")
      .rule(rule.getName)
      .arn(lambda.getArn)
      .targetId(s"$name-target-id")
  }

  def addLambdaPermission[D](
    name: String,
    function: LambdaFunction,
    source: CloudwatchEventRule
  ): TerraformStackBuildState[D, LambdaPermission] = buildAndAddResource { context =>
    LambdaPermission.Builder
      .create(context.stack, s"$name-lambda-permission")
      .statementId("AllowExecutionFromEventBridge")
      .action("lambda:InvokeFunction")
      .functionName(function.getFunctionName)
      .principal("events.amazonaws.com")
      .sourceArn(source.getArn)
  }

  def scheduleLambdaFunction[D](function: LambdaFunction, scheduleExpression: String) =
    for {
      rule <- addCloudwatchEventRule[D](function.getFunctionNameInput, scheduleExpression)
      permisssion <- addLambdaPermission(function.getFunctionNameInput, function, rule)
      target <- addCloudwatchEventTarget(function.getFunctionNameInput, function, rule)
    } yield rule

}
