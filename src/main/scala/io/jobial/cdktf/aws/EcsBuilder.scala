package io.jobial.cdktf.aws

import cats.data.State
import com.hashicorp.cdktf.providers.aws.ecs_cluster.EcsCluster
import com.hashicorp.cdktf.providers.aws.ecs_cluster.EcsClusterConfiguration
import com.hashicorp.cdktf.providers.aws.ecs_cluster_capacity_providers.EcsClusterCapacityProviders
import com.hashicorp.cdktf.providers.aws.ecs_service.EcsService
import com.hashicorp.cdktf.providers.aws.ecs_service.EcsServiceNetworkConfiguration
import com.hashicorp.cdktf.providers.aws.ecs_task_definition.EcsTaskDefinition
import com.hashicorp.cdktf.providers.aws.ecs_task_definition.EcsTaskDefinitionVolume
import com.hashicorp.cdktf.providers.aws.iam_role.IamRole
import io.circe.Json
import io.circe.generic.auto._
import io.circe.generic.extras.semiauto.deriveEnumerationEncoder
import io.circe.syntax._
import io.jobial.cdktf.util.json._

import scala.collection.JavaConverters._

trait EcsBuilder extends IamBuilder {

  def addCluster[D](
    name: String,
    configuration: Option[EcsClusterConfiguration],
    tags: Map[String, String]
  ): TerraformStackBuildState[D, EcsCluster] = buildAndAddResource[D, EcsCluster] { context =>
    val b = EcsCluster.Builder
      .create(context.stack, name)
      .name(name)
      .tags(context.mergeTags(name, tags).asJava)
    configuration.map(b.configuration)
    b
  }

  def addCluster[D](
    name: String,
    capacityProviders: List[String] = List("FARGATE"),
    tags: Map[String, String] = Map()
  ): TerraformStackBuildState[D, EcsCluster] =
    for {
      cluster <- addCluster(name, None, tags)
      capacityProviders <- addClusterCapacityProviders(s"$name-capacity-providers", name, capacityProviders)
    } yield cluster

  def addClusterCapacityProviders[D](
    name: String,
    clusterName: String,
    capacityProviders: List[String] = List("FARGATE")
  ) = buildAndAddResource[D, EcsClusterCapacityProviders] { context =>
    EcsClusterCapacityProviders.Builder
      .create(context.stack, name)
      .clusterName(clusterName)
      .capacityProviders(capacityProviders.asJava)
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
  ) = buildAndAddResource[D, EcsTaskDefinition] { context =>
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
      .tags((context.tags ++ tags).asJava)
  }

  def addService[D](
    name: String,
    cluster: EcsCluster,
    taskDefinition: EcsTaskDefinition,
    networkConfiguration: EcsServiceNetworkConfiguration,
    forceNewDeployment: Boolean = false,
    enableExecuteCommand: Boolean = true,
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, EcsService] { context =>
    EcsService.Builder
      .create(context.stack, name)
      .name(name)
      .enableExecuteCommand(enableExecuteCommand)
      .taskDefinition(taskDefinition.getId)
      .launchType("FARGATE")
      .cluster(cluster.getId)
      .desiredCount(1)
      .networkConfiguration(networkConfiguration)
      .forceNewDeployment(forceNewDeployment)
      .tags((context.tags ++ tags).asJava)
  }

  def addNetworkConfiguration[D](
    securityGroups: List[String],
    subnets: List[String]
  ) = buildAndAddResource[D, EcsServiceNetworkConfiguration] { context =>
    new EcsServiceNetworkConfiguration.Builder()
      .securityGroups(securityGroups.asJava)
      .subnets(subnets.asJava)
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
