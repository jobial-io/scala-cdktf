package io.jobial.cdktf.aws

import cats.Eval
import cats.data.IndexedStateT
import cats.data.State
import cats.effect.IO
import com.hashicorp.cdktf.App
import com.hashicorp.cdktf.AppConfig
import com.hashicorp.cdktf.S3Backend
import com.hashicorp.cdktf.TerraformStack
import com.hashicorp.cdktf.providers.aws.cloudwatch_log_group.CloudwatchLogGroup
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2Fleet
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfig
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigLaunchTemplateSpecification
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverride
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverrideInstanceRequirements
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverrideInstanceRequirementsMemoryMib
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverrideInstanceRequirementsVcpuCount
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetSpotOptions
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetTargetCapacitySpecification
import com.hashicorp.cdktf.providers.aws.ecs_cluster.EcsCluster
import com.hashicorp.cdktf.providers.aws.ecs_cluster.EcsClusterConfiguration
import com.hashicorp.cdktf.providers.aws.ecs_cluster.EcsClusterSetting
import com.hashicorp.cdktf.providers.aws.ecs_cluster_capacity_providers.EcsClusterCapacityProviders
import com.hashicorp.cdktf.providers.aws.ecs_service.EcsService
import com.hashicorp.cdktf.providers.aws.ecs_service.EcsServiceNetworkConfiguration
import com.hashicorp.cdktf.providers.aws.ecs_task_definition.EcsTaskDefinition
import com.hashicorp.cdktf.providers.aws.ecs_task_definition.EcsTaskDefinitionVolume
import com.hashicorp.cdktf.providers.aws.iam_instance_profile.IamInstanceProfile
import com.hashicorp.cdktf.providers.aws.iam_policy.IamPolicy
import com.hashicorp.cdktf.providers.aws.iam_role.IamRole
import com.hashicorp.cdktf.providers.aws.iam_role.IamRoleInlinePolicy
import com.hashicorp.cdktf.providers.aws.instance.Instance
import com.hashicorp.cdktf.providers.aws.instance.InstanceInstanceMarketOptions
import com.hashicorp.cdktf.providers.aws.instance.InstanceInstanceMarketOptionsSpotOptions
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplate
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateIamInstanceProfile
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateInstanceRequirements
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateInstanceRequirementsMemoryMib
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateInstanceRequirementsVcpuCount
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateNetworkInterfaces
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateTagSpecifications
import com.hashicorp.cdktf.providers.aws.provider.AwsProvider
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequest
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestLaunchSpecification
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestLaunchTemplateConfig
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestLaunchTemplateConfigLaunchTemplateSpecification
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestLaunchTemplateConfigOverrides
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirements
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirementsMemoryMib
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirementsVcpuCount
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestSpotMaintenanceStrategies
import com.hashicorp.cdktf.providers.aws.spot_fleet_request.SpotFleetRequestSpotMaintenanceStrategiesCapacityRebalance
import io.circe.Json
import io.circe.Json.obj
import io.circe.generic.auto._
import io.circe.generic.extras.semiauto.deriveEnumerationEncoder
import io.circe.syntax._
import io.jobial.cdktf.aws.TerraformStackBuildContext.CdktfNameTag
import io.jobial.cdktf.aws.TerraformStackBuildContext.CdktfTimestampTag
import io.jobial.cdktf.util.json._
import software.amazon.jsii.Builder

import java.time.Instant.now
import java.time.LocalDateTime
import scala.collection.JavaConverters._

case class TerraformStackBuildContext[D](
  name: String,
  stack: TerraformStack,
  app: App,
  data: D,
  containerDefinitions: Map[String, ContainerDefinition] = Map(),
  tags: Map[String, String] = Map()
) {
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

  def mergeTags(resourceName: Option[String], tags: Map[String, String]) =
    this.tags ++ tags ++
      (resourceName match {
        case Some(resourceName) =>
          Map(CdktfNameTag -> resourceName)
        case None =>
          Map()
      }) +
      (CdktfTimestampTag -> now.toString)

  def mergeTags(resourceName: String, tags: Map[String, String]): Map[String, String] =
    mergeTags(Some(resourceName), tags)

  def mergeTags(tags: Map[String, String]): Map[String, String] =
    mergeTags(None, tags)
}

object TerraformStackBuildContext {

  def apply[D](name: String, data: D, appContext: Map[String, _], tags: Map[String, String]) = {
    val config = AppConfig
      .builder
      .context(appContext.asJava)
      .build
    val app = new App(config)
    new TerraformStackBuildContext(name, new TerraformStack(app, name), app, data, tags = tags)
  }

  val CdktfNameTag = "cdktf:name"
  val CdktfTimestampTag = "cdktf:timestamp"
}

trait TerraformStackBuilder {

  type TerraformStackBuildState[D, A] = State[TerraformStackBuildContext[D], A]

  def createStack(name: String)(state: TerraformStackBuildState[Unit, Unit]) =
    createStack[Unit](name, ())(state)

  def createStack[D](name: String, data: D, appContext: Map[String, _] = defaultAppContext, tags: Map[String, String] = Map())(state: TerraformStackBuildState[D, Unit]) =
    state.run(TerraformStackBuildContext(name, data, appContext, tags)).value._1

  def defaultAppContext = Map(
    "excludeStackIdFromLogicalIds" -> true,
    "allowSepCharsInLogicalIds" -> true
  )

  def addResource[D, T](resource: T): TerraformStackBuildState[D, T] =
    State.inspect { context =>
      resource
    }

  def buildAndAddResource[D, T](builder: TerraformStackBuildContext[D] => Builder[T], postBuild: (TerraformStackBuildContext[D], T) => T = (_: TerraformStackBuildContext[D], resource: T) => resource): TerraformStackBuildState[D, T] =
    for {
      context <- getContext
      resource <- addResource(builder(context).build)
    } yield postBuild(context, resource)

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

  def addInstance[D](
    name: String,
    ami: String,
    instanceType: String,
    securityGroups: List[String],
    subnetId: String,
    spotInstanceType: String = "one-time",
    userData: Option[String] = None,
    maxPrice: Option[Double] = None,
    validUntil: Option[LocalDateTime] = None,
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, Instance] { context =>
    val b = Instance.Builder
      .create(context.stack, name)
      .ami(ami)
      .instanceType(instanceType)
      .securityGroups(securityGroups.asJava)
      .subnetId(subnetId)
      .tags((context.tags ++ tags).asJava)
    userData.map(b.userData)
    maxPrice.map(maxPrice =>
      b.instanceMarketOptions(
        InstanceInstanceMarketOptions
          .builder
          .marketType("spot")
          .spotOptions {
            val b = InstanceInstanceMarketOptionsSpotOptions
              .builder
              .spotInstanceType(spotInstanceType)
              .maxPrice(maxPrice.toString)
            validUntil.map(d => b.validUntil(d.toString))
            b.build
          }
          .build
      )
    )
    b
  }

  def addS3Backend[D](bucket: String, key: String) = buildAndAddResource[D, S3Backend] { context =>
    S3Backend.Builder
      .create(context.stack)
      .bucket(bucket)
      .key(key)
  }

  def addAwsProvider[D](region: String) = buildAndAddResource[D, AwsProvider] { context =>
    AwsProvider.Builder
      .create(context.stack, "AWS")
      .region(region)
  }

  def addLogGroup[D](name: String, skipDestroy: Boolean = true) = buildAndAddResource[D, CloudwatchLogGroup] { context =>
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

  def addSpotFleetRequest[D](
    name: String,
    iamFleetRole: String,
    spotPrice: Double,
    targetCapacity: Int,
    onDemandCapacity: Int = 0,
    terminateInstancesWithExpiration: Boolean = true,
    terminateInstancesOnDelete: Boolean = true,
    validUntil: Option[LocalDateTime] = None,
    fleetType: String = "maintain",
    allocationStrategy: String = "lowestPrice",
    instanceInterruptionBehaviour: String = "stop",
    replaceUnhealthyInstances: Boolean = true,
    launchTemplateConfigs: List[SpotFleetRequestLaunchTemplateConfig] = List(),
    launchSpecifications: List[SpotFleetRequestLaunchSpecification] = List(),
    availabilityZone: Option[String] = None,
    tags: Map[String, String] = Map()
  ): TerraformStackBuildState[D, SpotFleetRequest] = buildAndAddResource[D, SpotFleetRequest]({ context =>
    val b = SpotFleetRequest.Builder
      .create(context.stack, name)
      .iamFleetRole(iamFleetRole)
      .fleetType(fleetType)
      .allocationStrategy(allocationStrategy)
      .spotMaintenanceStrategies(SpotFleetRequestSpotMaintenanceStrategies.builder
        .capacityRebalance(
          SpotFleetRequestSpotMaintenanceStrategiesCapacityRebalance.builder.replacementStrategy("launch").build
        ).build
      )
      .spotPrice(spotPrice.toString)
      .launchSpecification(launchSpecifications.asJava)
      .launchTemplateConfig(launchTemplateConfigs.asJava)
      .targetCapacity(targetCapacity)
      .onDemandTargetCapacity(onDemandCapacity)
      .terminateInstancesWithExpiration(terminateInstancesWithExpiration)
      .terminateInstancesOnDelete(terminateInstancesOnDelete.toString)
      .instanceInterruptionBehaviour(instanceInterruptionBehaviour)
      //.excessCapacityTerminationPolicy()
      .replaceUnhealthyInstances(replaceUnhealthyInstances)
      .tags(context.mergeTags(name, tags).asJava)
    validUntil.map(d => b.validUntil(d.toString))
    b
  }, { (context, r) =>
    // adding override for terraform bug
    for {
      _ <- launchTemplateConfigs.headOption
      availabilityZone <- availabilityZone
    } r.addOverride("launch_template_config.availability_zone", availabilityZone)
    r
  })

  def addSpotFleetRequest[D](
    name: String,
    iamFleetRole: String,
    spotPrice: Double,
    targetCapacity: Int,
    imageId: String,
    instanceTypes: List[String],
    subnetId: String,
    securityGroups: List[String],
    keyName: String,
    tags: Map[String, String]
  ): TerraformStackBuildState[D, SpotFleetRequest] = for {
    launchTemplate <- addLaunchTemplate[D](
      s"$name-launch-template",
      imageId,
      subnetId,
      securityGroups,
      keyName,
      instanceRequirements = Some(
        LaunchTemplateInstanceRequirements
          .builder
          .allowedInstanceTypes(instanceTypes.asJava)
          .memoryMib(
            LaunchTemplateInstanceRequirementsMemoryMib
              .builder
              .min(1000)
              .build
          )
          .vcpuCount(
            LaunchTemplateInstanceRequirementsVcpuCount
              .builder
              .min(1)
              .build
          )
          .build
      ),
      tags = tags
    )
    launchTemplateConfig = spotFleetRequestLaunchTemplateConfig(launchTemplate)
    spotFleetRequest <- addSpotFleetRequest(
      name,
      iamFleetRole,
      spotPrice,
      targetCapacity,
      launchTemplateConfigs = List(launchTemplateConfig),
      tags = tags
    )
  } yield spotFleetRequest

  def sequence[S, T](l: List[State[S, T]]): State[S, List[T]] =
    l.headOption match {
      case Some(h) =>
        for {
          h <- h
          t <- sequence(l.tail)
        } yield h :: t
      case None =>
        State.inspect[S, List[T]](_ => List())
    }

  def addLaunchTemplate[D](
    name: String,
    imageId: String,
    subnetId: String,
    securityGroups: List[String],
    keyName: String,
    instanceType: Option[String] = None,
    instanceRequirements: Option[LaunchTemplateInstanceRequirements] = None,
    instanceProfile: Option[IamInstanceProfile] = None,
    userData: Option[String] = None,
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, LaunchTemplate] { context =>
    val b = LaunchTemplate.Builder
      .create(context.stack, name)
      .imageId(imageId)
      .networkInterfaces(List(
        LaunchTemplateNetworkInterfaces.builder
          .securityGroups(securityGroups.asJava)
          .subnetId(subnetId)
          .build
      ).asJava)
      .keyName(keyName)
      .tags(context.mergeTags(name, tags).asJava)
      .tagSpecifications(List(
        LaunchTemplateTagSpecifications
          .builder
          .resourceType("instance")
          .tags(context.mergeTags(name, tags).asJava)
          .build
      ).asJava)

    instanceType.map(b.instanceType)
    instanceRequirements.map(b.instanceRequirements)
    instanceProfile.map(p => b.iamInstanceProfile(LaunchTemplateIamInstanceProfile.builder
      .name(s"$name-launch-template-instance-profile")
      .arn(p.getArn)
      .build)
    )
    userData.map(b.userData)
    b
  }

  def addIamInstanceProfile[D](
    name: String,
    role: IamRole
  ) = buildAndAddResource[D, IamInstanceProfile] { context =>
    IamInstanceProfile.Builder
      .create(context.stack, name)
      .role(role.getName)
  }

  def spotFleetRequestLaunchSpecification[D](
    ami: String,
    instanceType: String,
    spotPrice: Double,
    securityGroups: List[String],
    keyName: String,
    subnetId: String,
    availabilityZone: String,
    monitoring: Boolean = false,
    tags: Map[String, String] = Map()
  ): TerraformStackBuildState[D, SpotFleetRequestLaunchSpecification] =
    for {
      context <- getContext
    } yield
      SpotFleetRequestLaunchSpecification.builder
        .ami(ami)
        .keyName(keyName)
        .spotPrice(spotPrice.toString)
        .subnetId(subnetId)
        .availabilityZone(availabilityZone)
        .instanceType(instanceType)
        .vpcSecurityGroupIds(securityGroups.asJava)
        .subnetId(subnetId)
        .monitoring(monitoring)
        .tags(context.mergeTags(tags).asJava)
        .build

  def spotFleetRequestLaunchTemplateConfig(
    launchTemplate: LaunchTemplate,
    version: String = "$Latest",
    overrides: Option[List[SpotFleetRequestLaunchTemplateConfigOverrides]] = None
  ) = {
    val b = SpotFleetRequestLaunchTemplateConfig.builder()
      .launchTemplateSpecification(SpotFleetRequestLaunchTemplateConfigLaunchTemplateSpecification
        .builder
        .name(launchTemplate.getName)
        .version(version)
        .build
      )
    overrides.map(o => b.overrides(o.asJava))
    b.build
  }

  def spotFleetRequestLaunchTemplateConfigOverrides =
    SpotFleetRequestLaunchTemplateConfigOverrides.builder
      .instanceRequirements(
        SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirements.builder
          .vcpuCount(
            SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirementsVcpuCount.builder
              .min(1)
              .max(64)
              .build
          )
          .memoryMib(
            SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirementsMemoryMib.builder
              .min(1)
              .max(100000)
              .build
          )
          .build
      ).build


  def addEc2Fleet[D](
    name: String,
    //iamFleetRole: String,
    spotPrice: Double,
    targetCapacity: Int,
    imageId: String,
    instanceTypes: List[String],
    subnetId: String,
    availabilityZone: String,
    securityGroups: List[String],
    keyName: String,
    tags: Map[String, String]
  ): TerraformStackBuildState[D, Ec2Fleet] = for {
    launchTemplate <- addLaunchTemplate[D](
      s"$name-launch-template",
      imageId,
      subnetId,
      securityGroups,
      keyName,
      instanceRequirements = Some(
        LaunchTemplateInstanceRequirements
          .builder
          .allowedInstanceTypes(instanceTypes.asJava)
          .memoryMib(
            LaunchTemplateInstanceRequirementsMemoryMib
              .builder
              .min(1000)
              .build
          )
          .vcpuCount(
            LaunchTemplateInstanceRequirementsVcpuCount
              .builder
              .min(1)
              .build
          )
          .build
      ),
      tags = tags
    )
    launchTemplateConfig = ec2FleetLaunchTemplateConfig(
      launchTemplate,
      overrides = List(ec2FleetLaunchTemplateConfigOverride(
        subnetId,
        spotPrice,
        instanceTypes,
        availabilityZone
      ))
    )
    fleet <- addEc2Fleet(
      name,
      List(launchTemplateConfig),
      targetCapacity,
      tags = tags
    )
  } yield fleet

  def ec2FleetLaunchTemplateConfig(
    launchTemplate: LaunchTemplate,
    version: String = "$Latest",
    overrides: List[Ec2FleetLaunchTemplateConfigOverride] = List()
  ) =
    Ec2FleetLaunchTemplateConfig
      .builder
      .launchTemplateSpecification(
        Ec2FleetLaunchTemplateConfigLaunchTemplateSpecification
          .builder
          .launchTemplateName(launchTemplate.getName)
          .version(version)
          .build
      )
      .`override`(overrides.asJava)
      .build

  def ec2FleetLaunchTemplateConfigOverride(
    subnetId: String,
    maxPrice: Double,
    instanceTypes: List[String],
    availabilityZone: String
  ) =
    Ec2FleetLaunchTemplateConfigOverride
      .builder
      .subnetId(subnetId)
      .availabilityZone(availabilityZone)
      .maxPrice(maxPrice.toString)
      .instanceRequirements(
        Ec2FleetLaunchTemplateConfigOverrideInstanceRequirements
          .builder
          .allowedInstanceTypes(instanceTypes.asJava)
          .memoryMib(
            Ec2FleetLaunchTemplateConfigOverrideInstanceRequirementsMemoryMib
              .builder
              .min(1)
              .build
          )
          .vcpuCount(
            Ec2FleetLaunchTemplateConfigOverrideInstanceRequirementsVcpuCount
              .builder
              .min(1)
              .build
          )
          .build
      )
      .build

  def addEc2Fleet[D](
    name: String,
    launchTemplateConfigs: List[Ec2FleetLaunchTemplateConfig],
    spotTargetCapacity: Int,
    onDemandTargetCapacity: Int = 0,
    terminateInstancesWithExpiration: Boolean = true,
    terminateInstancesOnDelete: Boolean = true,
    validUntil: Option[LocalDateTime] = None,
    fleetType: String = "maintain",
    allocationStrategy: String = "lowestPrice",
    instanceInterruptionBehaviour: String = "stop",
    replaceUnhealthyInstances: Boolean = true,
    tags: Map[String, String] = Map()
  ): TerraformStackBuildState[D, Ec2Fleet] = buildAndAddResource { context =>
    val b = Ec2Fleet.Builder
      .create(context.stack, name)
      .launchTemplateConfig(launchTemplateConfigs.asJava)
      .spotOptions(
        Ec2FleetSpotOptions
          .builder
          .allocationStrategy(allocationStrategy)
          .instanceInterruptionBehavior(instanceInterruptionBehaviour)
          .build
      )
      .`type`(fleetType)
      .terminateInstancesWithExpiration(terminateInstancesWithExpiration)
      .terminateInstances(terminateInstancesOnDelete)
      .replaceUnhealthyInstances(replaceUnhealthyInstances)
      .targetCapacitySpecification(
        Ec2FleetTargetCapacitySpecification
          .builder
          .defaultTargetCapacityType("spot")
          .spotTargetCapacity(spotTargetCapacity)
          .onDemandTargetCapacity(onDemandTargetCapacity)
          .totalTargetCapacity(spotTargetCapacity + onDemandTargetCapacity)
          .build
      )
      .tags(context.mergeTags(name, tags).asJava)
    validUntil.map(d => b.validUntil(d.toString))
    b
  }

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
  //
  //  def addMyResource[D](
  //    name: String
  //  ): TerraformStackBuildState[D, MyResource] =
  //    State.inspect { context =>
  //      new MyResource(context.stack)
  //    }

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

//class MyResource(scope: Construct) extends TerraformResource(software.amazon.jsii.JsiiObject.InitializationMode.JSII) {
//  software.amazon.jsii.JsiiEngine.getInstance.createNewObject(this, Array[AnyRef](java.util.Objects.requireNonNull(scope, "scope is required"), java.util.Objects.requireNonNull("hello", "id is required"), java.util.Objects.requireNonNull("", "config is required")))
//}
