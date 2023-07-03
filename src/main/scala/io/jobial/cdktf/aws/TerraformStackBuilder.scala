package io.jobial.cdktf.aws

import cats.data.State
import cats.effect.IO
import io.jobial.cdktf.util.json._
import com.hashicorp.cdktf.App
import com.hashicorp.cdktf.S3Backend
import com.hashicorp.cdktf.TerraformStack
import com.hashicorp.cdktf.providers.aws.cloudwatch_log_group.CloudwatchLogGroup
import com.hashicorp.cdktf.providers.aws.ecs_cluster.EcsCluster
import com.hashicorp.cdktf.providers.aws.ecs_service.EcsService
import com.hashicorp.cdktf.providers.aws.ecs_service.EcsServiceNetworkConfiguration
import com.hashicorp.cdktf.providers.aws.ecs_task_definition.EcsTaskDefinition
import com.hashicorp.cdktf.providers.aws.ecs_task_definition.EcsTaskDefinitionVolume
import com.hashicorp.cdktf.providers.aws.iam_policy.IamPolicy
import com.hashicorp.cdktf.providers.aws.iam_role.IamRole
import com.hashicorp.cdktf.providers.aws.iam_role.IamRoleInlinePolicy
import com.hashicorp.cdktf.providers.aws.instance.Instance
import com.hashicorp.cdktf.providers.aws.provider.AwsProvider
import io.circe.Json
import io.circe.Json.arr
import io.circe.Json.obj
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.generic.extras.semiauto.deriveEnumerationEncoder
import software.amazon.jsii.Builder

import scala.collection.JavaConverters._
import scala.collection.immutable.List

case class TerraformStackBuildContext[D](stack: TerraformStack, app: App, data: D, containerDefinitions: Map[String, ContainerDefinition] = Map()) {

  def synth = IO(app.synth())

  def updateData(data: D) = copy(data = data)

  def addCointainerDefinition(containerDefinition: ContainerDefinition) =
    copy(containerDefinitions = containerDefinitions + (containerDefinition.name -> containerDefinition))

  def containerDefinitionsWithTransitiveDependencies(definitions: Iterable[ContainerDefinition]): List[ContainerDefinition] = {
    val names = definitions.map(_.name).toSet
    val closureNames = definitions.flatMap(_.dependsOn.map(_.containerName)).toSet ++ names
    val closure = containerDefinitions.values.filter(d => closureNames.contains(d.name)).toList
    if (names == closureNames)
      closure
    else
      containerDefinitionsWithTransitiveDependencies(closure)
  }
}

object TerraformStackBuildContext {

  def apply[D](name: String, data: D) = {
    val app = new App
    new TerraformStackBuildContext(new TerraformStack(app, name), app, data)
  }
}

trait TerraformStackBuilder {

  type TerraformStackBuildState[D, A] = State[TerraformStackBuildContext[D], A]

  def createStack(name: String)(state: TerraformStackBuildState[Unit, Unit]) =
    createStack[Unit](name, ())(state)

  def createStack[D](name: String, data: D)(state: TerraformStackBuildState[D, Unit]) =
    state.run(TerraformStackBuildContext(name, data)).value._1.synth

  def addResource[D, T](builder: TerraformStackBuildContext[D] => Builder[T]): TerraformStackBuildState[D, T] =
    State.inspect { context =>
      builder(context).build
    }

  def addCluster[D](
    name: String,
    capacityProviders: List[String] = List("FARGATE"),
    tags: Map[String, String] = Map()
  ) = addResource[D, EcsCluster] { context =>
    EcsCluster.Builder
      .create(context.stack, name)
      .name(name)
      .capacityProviders(capacityProviders.asJava)
      //.setting()
      .tags(tags.asJava)
  }

  def addContainerDefinition[D](
    containerDefinition: ContainerDefinition
  ) =
    State.modify[TerraformStackBuildContext[D]](_.addCointainerDefinition(containerDefinition))
      .inspect(_ => containerDefinition)

  def volume(name: String) =
    new EcsTaskDefinitionVolume.Builder()
      .name(name)
      .build()

  def addTaskDefinition[D](
    name: String,
    containerDefinitions: List[ContainerDefinition],
    taskRole: IamRole,
    executionRole: IamRole,
    volume: List[EcsTaskDefinitionVolume] = List(),
    cpu: Int = 1024,
    memory: Int = 2048,
    tags: Map[String, String] = Map(),
    awslogsStreamPrefixOverride: Option[String] = None
  ) = addResource[D, EcsTaskDefinition] { context =>
    val definitionsWithDependencies = context.containerDefinitionsWithTransitiveDependencies(containerDefinitions)
    EcsTaskDefinition.Builder
      .create(context.stack, name)
      .family(name)
      .containerDefinitions(awslogsStreamPrefixOverride.map(prefix => definitionsWithDependencies.map(_.setAwslogsStreamPrefix(prefix))).getOrElse(definitionsWithDependencies).map(_.asJson).noSpaces)
      .taskRoleArn(taskRole.getArn)
      .executionRoleArn(executionRole.getArn)
      .requiresCompatibilities(List("FARGATE").asJava)
      .networkMode("awsvpc")
      .volume(volume.asJava)
      .cpu(cpu.toString)
      .memory(memory.toString)
      .tags(tags.asJava)
  }

  def addService[D](
    name: String,
    cluster: EcsCluster,
    taskDefinition: EcsTaskDefinition,
    networkConfiguration: EcsServiceNetworkConfiguration,
    forceNewDeployment: Boolean = false,
    tags: Map[String, String] = Map()
  ) = addResource[D, EcsService] { context =>
    EcsService.Builder
      .create(context.stack, name)
      .name(name)
      .taskDefinition(taskDefinition.getId)
      .launchType("FARGATE")
      .cluster(cluster.getId)
      .desiredCount(1)
      .networkConfiguration(networkConfiguration)
      .forceNewDeployment(forceNewDeployment)
      .tags(tags.asJava)
  }

  def addNetworkConfiguration[D](
    securityGroups: List[String],
    subnets: List[String]
  ) = addResource[D, EcsServiceNetworkConfiguration] { context =>
    new EcsServiceNetworkConfiguration.Builder()
      .securityGroups(securityGroups.asJava)
      .subnets(subnets.asJava)
  }

  def addInstance[D](
    name: String,
    ami: String,
    instanceType: String,
    securityGroups: List[String],
    subnetId: String,
    tags: Map[String, String] = Map()
  ) = addResource[D, Instance] { context =>
    Instance.Builder
      .create(context.stack, name)
      .ami(ami)
      .instanceType(instanceType)
      .securityGroups(securityGroups.asJava)
      .subnetId(subnetId)
      .tags(tags.asJava)
  }

  def addS3Backend[D](bucket: String, key: String) = addResource[D, S3Backend] { context =>
    S3Backend.Builder
      .create(context.stack)
      .bucket(bucket)
      .key(key)
  }

  def addAwsProvider[D](region: String) = addResource[D, AwsProvider] { context =>
    AwsProvider.Builder
      .create(context.stack, "AWS")
      .region(region)
  }

  def addLogGroup[D](name: String, skipDestroy: Boolean = true) = addResource[D, CloudwatchLogGroup] { context =>
    CloudwatchLogGroup.Builder
      .create(context.stack, s"$name-loggroup")
      .name(name)
      .skipDestroy(skipDestroy)
  }

  def addEcsTaskExecutionRole[D](
    name: String,
    managedPolicyArns: List[String] = List("arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"),
    assumeRolePolicy: Json = DefaultAssumeRolePolicy
  ) = addRole[D](
    name,
    managedPolicyArns = managedPolicyArns,
    assumeRolePolicy = Some(assumeRolePolicy)
  )

  def addEcsTaskRole[D](
    name: String,
    managedPolicyArns: List[String],
    assumeRolePolicy: Json = DefaultAssumeRolePolicy
  ) = addRole[D](
    name,
    assumeRolePolicy = Some(assumeRolePolicy),
    managedPolicyArns = managedPolicyArns
  )

  def addRole[D](
    name: String,
    managedPolicyArns: List[String] = List(),
    inlinePolicy: List[IamRoleInlinePolicy] = List(),
    assumeRolePolicy: Option[Json] = None
  ) = addResource[D, IamRole] { context =>
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

  def addPolicy[D](name: String, policy: Json) = addResource[D, IamPolicy] { context =>
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

  def getContext[D]: TerraformStackBuildState[D, TerraformStackBuildContext[D]] = State.inspect { context =>
    context
  }

  def getData[D]: TerraformStackBuildState[D, D] =
    getContext.map(_.data)

  def updateData[D](f: D => D) =
    for {
      data <- getData[D]
      r <- State.modify[TerraformStackBuildContext[D]](_.updateData(f(data)))
    } yield r

  implicit val containerDependencyConditionEncoder = deriveEnumerationEncoder[ContainerDependencyCondition]

  def logConfiguration(
    awslogsGroup: String,
    awslogsRegion: String,
    awslogsStreamPrefix: String,
    awslogsCreateGroup: Boolean = false
  ) =
    LogConfiguration(
      LogOptions(
        Some(awslogsGroup),
        Some(awslogsRegion),
        Some(awslogsStreamPrefix),
        if (awslogsCreateGroup) Some("true") else None
      )
    )
}

sealed trait ContainerDependencyCondition

case object START extends ContainerDependencyCondition

case object COMPLETE extends ContainerDependencyCondition

case object SUCCESS extends ContainerDependencyCondition

case object HEALTHY extends ContainerDependencyCondition

case class ContainerDefinition(
  name: String,
  image: String,
  command: List[String],
  entrypoint: Option[List[String]] = None,
  essential: Boolean = true,
  dependsOn: List[ContainerDependency] = List(),
  logConfiguration: LogConfiguration = LogConfiguration(),
  mountPoints: List[MountPoint] = List(),
  cpu: Option[Int] = None,
  memory: Option[Int] = None
) {

  def setAwslogsStreamPrefix(prefix: String) = copy(
    logConfiguration = logConfiguration.copy(
      options = logConfiguration.options.copy(`awslogs-stream-prefix` = Some(prefix))
    )
  )
}

case class ContainerDependency(
  containerName: String,
  condition: ContainerDependencyCondition
)

case class MountPoint(
  sourceVolume: String,
  containerPath: String
)

case class LogConfiguration(
  options: LogOptions = LogOptions(),
  logDriver: String = "awslogs"
)

case class LogOptions(
  `awslogs-group`: Option[String] = None,
  `awslogs-region`: Option[String] = None,
  `awslogs-stream-prefix`: Option[String] = None,
  `awslogs-create-group`: Option[String] = None
)