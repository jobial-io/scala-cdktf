package io.jobial.cdktf.aws

import com.hashicorp.cdktf.providers.aws.iam_policy.IamPolicy
import com.hashicorp.cdktf.providers.aws.iam_role.IamRole
import com.hashicorp.cdktf.providers.aws.iam_role.IamRoleInlinePolicy
import io.circe.Json
import io.circe.Json.obj
import io.circe.syntax._
import io.jobial.cdktf.util.json._

import scala.collection.JavaConverters._

trait IamBuilder {
  this: TerraformStackBuilder =>

  def addRole[D](
    name: String,
    managedPolicyArns: List[String] = List(),
    inlinePolicy: List[IamRoleInlinePolicy] = List(),
    assumeRolePolicy: Option[Json] = None
  ) = buildAndAddResource[D, IamRole] { context =>
    val builder = IamRole.Builder
      .create(context.stack, name)
      .managedPolicyArns(managedPolicyArns.asJava)
      .inlinePolicy(inlinePolicy.asJava)
    assumeRolePolicy.map(p => builder.assumeRolePolicy(p.noSpaces))
    builder
  }

  def policy(action: List[String], resource: List[String]): Json =
    policy(
      statement(
        action,
        resource
      )
    )

  def policy(service: String, action: String): Json =
    policy(
      statement(
        List(action),
        List(),
        principal = Some(obj(
          "Service" -> service
        ))
      )
    )

  def addPolicy[D](name: String, policy: Json) = buildAndAddResource[D, IamPolicy] { context =>
    IamPolicy.Builder
      .create(context.stack, name)
      .name(name)
      .policy(policy.noSpaces)
  }

  def inlinePolicy(policy: Json) =
    new IamRoleInlinePolicy.Builder()
      .policy(policy.noSpaces)
      .build

  def policy(statement: Json): Json =
    policy(List(statement))

  def statement(actions: String*): Json =
    statement(actions.toList, List("*"))

  def statement(action: List[String], resource: List[String], effect: String = "Allow", principal: Option[Json] = None, sid: Option[String] = None) =
    obj(
      (List(
        "Effect" -> effect,
        "Action" -> action
      ): List[(String, Json)]) ++ sid.map(_ =>
        "Sid" -> sid.asJson
      ) ++ resource.headOption.map(_ =>
        "Resource" -> resource.asJson
      ) ++ principal.map(_ =>
        "Principal" -> principal.asJson
      ): _*
    )

  def policy(statements: List[Json]): Json =
    obj(
      "Version" -> "2012-10-17",
      "Statement" -> statements
    )

  val DefaultAssumeRolePolicy = policy("ecs-tasks.amazonaws.com", "sts:AssumeRole")

  val DefaultPolicy = inlinePolicy(
    policy(
      List(
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutLogEvents",
        "logs:DescribeLogStreams"
      ),
      List(
        "arn:aws:logs:*:*:*"
      )
    )
  )

  val SecretsPolicy = inlinePolicy(
    policy(
      List(
        "secretsmanager:GetSecretValue"
      ),
      List(
        "*"
      )
    )
  )

  val SessionManagerAccessPolicy = policy(
    statement(
      "ssmmessages:CreateControlChannel",
      "ssmmessages:CreateDataChannel",
      "ssmmessages:OpenControlChannel",
      "ssmmessages:OpenDataChannel"
    )
  )

}
