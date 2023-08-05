package io.jobial.cdktf.aws

import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2Fleet
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfig
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigLaunchTemplateSpecification
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverride
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverrideInstanceRequirements
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverrideInstanceRequirementsMemoryMib
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetLaunchTemplateConfigOverrideInstanceRequirementsVcpuCount
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetSpotOptions
import com.hashicorp.cdktf.providers.aws.ec2_fleet.Ec2FleetTargetCapacitySpecification
import com.hashicorp.cdktf.providers.aws.iam_instance_profile.IamInstanceProfile
import com.hashicorp.cdktf.providers.aws.iam_role.IamRole
import com.hashicorp.cdktf.providers.aws.instance.Instance
import com.hashicorp.cdktf.providers.aws.instance.InstanceInstanceMarketOptions
import com.hashicorp.cdktf.providers.aws.instance.InstanceInstanceMarketOptionsSpotOptions
import com.hashicorp.cdktf.providers.aws.instance.InstanceMetadataOptions
import com.hashicorp.cdktf.providers.aws.instance.InstanceRootBlockDevice
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplate
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateIamInstanceProfile
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateInstanceRequirements
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateInstanceRequirementsMemoryMib
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateInstanceRequirementsVcpuCount
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateNetworkInterfaces
import com.hashicorp.cdktf.providers.aws.launch_template.LaunchTemplateTagSpecifications
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

import java.time.LocalDateTime
import scala.collection.JavaConverters._

trait Ec2Builder {
  this: TerraformStackBuilder =>
  
  def addInstance[D](
    name: String,
    ami: String,
    instanceType: String,
    securityGroups: List[String],
    keyName: String,
    subnetId: String,
    spotInstanceType: String = "one-time",
    instanceInterruptionBehavior: String = "stop",
    hibernation: Boolean = false,
    userData: Option[String] = None,
    maxPrice: Option[Double] = None,
    rootEncrypted: Boolean = true,
    rootVolumeType: String = "gp3",
    rootVolumeSize: Int = 100,
    rootDeleteOnTermination: Boolean = true,
    rootThroughput: Int = 300,
    rootIOPS: Int = 3000,
    validUntil: Option[LocalDateTime] = None,
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, Instance] { context =>
    val b = Instance.Builder
      .create(context.stack, name)
      .ami(ami)
      .rootBlockDevice(
        InstanceRootBlockDevice
          .builder
          .encrypted(rootEncrypted)
          .volumeType(rootVolumeType)
          .volumeSize(rootVolumeSize)
          .deleteOnTermination(rootDeleteOnTermination)
          .throughput(rootThroughput)
          .iops(rootIOPS)
          .build
      )
      .instanceType(instanceType)
      .hibernation(hibernation)
      .metadataOptions(
        InstanceMetadataOptions
          .builder
          .httpTokens("optional")
          .build
      )
      .keyName(keyName)
//      .securityGroups(securityGroups.asJava)
      .vpcSecurityGroupIds(securityGroups.asJava)
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
              .instanceInterruptionBehavior(instanceInterruptionBehavior)
              .maxPrice(maxPrice.toString)
            validUntil.map(d => b.validUntil(d.toString))
            b.build
          }
          .build
      )
    )
    b
  }

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
    SpotFleetRequestLaunchTemplateConfigOverrides
      .builder
      .instanceRequirements(
        SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirements
          .builder
          .vcpuCount(
            SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirementsVcpuCount
              .builder
              .min(1)
              .max(64)
              .build
          )
          .memoryMib(
            SpotFleetRequestLaunchTemplateConfigOverridesInstanceRequirementsMemoryMib
              .builder
              .min(1)
              .max(100000)
              .build
          )
          .build
      ).build

  def addEc2Fleet[D](
    name: String,
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

}
