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
import com.hashicorp.cdktf.providers.aws.rds_cluster.RdsCluster
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
import io.jobial.cdktf.aws.TerraformStackBuildContext.NameTag

import java.time.LocalDateTime
import scala.collection.JavaConverters._

trait RdsBuilder extends IamBuilder {

  def addRdsCluster[D](
    name: String,
    engine: String,
    availabilityZones: List[String],
    masterUsername: String,
    masterPassword: String,
    vpcSecurityGroupIds: Option[List[String]] = None,
    databaseName: Option[String] = None,
    backupRetentionPeriod: Int = 5,
    preferredBackupWindow: String = "04:00-06:00",
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, RdsCluster] { context =>
    val b = RdsCluster.Builder
      .create(context.stack, name)
      .clusterIdentifier(name)
      .engine(engine)
      .availabilityZones(availabilityZones.asJava)
      .databaseName(databaseName.getOrElse(name.replace('-', '_')))
      .masterUsername(masterUsername)
      .masterPassword(masterPassword)
      .backupRetentionPeriod(backupRetentionPeriod)
      .preferredBackupWindow(preferredBackupWindow)
      .tags(context.mergeTags(name, tags).asJava)
    vpcSecurityGroupIds.map(vpcSecurityGroupIds => b.vpcSecurityGroupIds(vpcSecurityGroupIds.asJava))
    b
  }
}
