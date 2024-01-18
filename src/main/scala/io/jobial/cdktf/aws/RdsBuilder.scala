package io.jobial.cdktf.aws

import com.hashicorp.cdktf.providers.aws.db_instance.DbInstance
import com.hashicorp.cdktf.providers.aws.db_subnet_group.DbSubnetGroup
import com.hashicorp.cdktf.providers.aws.rds_cluster.RdsCluster
import com.hashicorp.cdktf.providers.aws.rds_cluster.RdsClusterScalingConfiguration
import com.hashicorp.cdktf.providers.aws.rds_cluster.RdsClusterServerlessv2ScalingConfiguration
import com.hashicorp.cdktf.providers.aws.rds_cluster_instance.RdsClusterInstance
import com.hashicorp.cdktf.providers.aws.rds_cluster_parameter_group.RdsClusterParameterGroup
import com.hashicorp.cdktf.providers.aws.rds_cluster_parameter_group.RdsClusterParameterGroupParameter

import scala.collection.JavaConverters._

trait RdsBuilder extends IamBuilder {

  def addDbSubnetGroup[D](
    name: String,
    subnetIds: List[String]
  ) =
    buildAndAddResource[D, DbSubnetGroup] { context =>
      DbSubnetGroup.Builder
        .create(context.stack, s"$name-db-subnet-group")
        .subnetIds(subnetIds.asJava)
    }

  def addRdsCluster[D](
    name: String,
    engine: String,
    engineMode: Option[String],
    availabilityZones: List[String],
    masterUsername: Option[String],
    masterPassword: Option[String],
    subnetGroup: DbSubnetGroup,
    clusterParameterGroup: RdsClusterParameterGroup,
    scalingConfiguration: Option[RdsClusterScalingConfiguration] = None,
    serverlessv2ScalingConfiguration: Option[RdsClusterServerlessv2ScalingConfiguration] = None,
    skipFinalSnapshot: Boolean = false,
    vpcSecurityGroupIds: Option[List[String]] = None,
    databaseName: Option[String] = None,
    backupRetentionPeriod: Int = 5,
    preferredBackupWindow: String = "04:00-06:00",
    snapshotIdentifier: Option[String] = None,
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, RdsCluster] { context =>
    val b = RdsCluster.Builder
      .create(context.stack, s"$name-rds-cluster")
      .clusterIdentifier(name)
      .engine(engine)
      .availabilityZones(availabilityZones.asJava)
      .databaseName(databaseName.getOrElse(name.replace('-', '_')))
      .backupRetentionPeriod(backupRetentionPeriod)
      .preferredBackupWindow(preferredBackupWindow)
      .skipFinalSnapshot(skipFinalSnapshot)
      .tags(context.mergeTags(name, tags).asJava)
    vpcSecurityGroupIds.map(vpcSecurityGroupIds => b.vpcSecurityGroupIds(vpcSecurityGroupIds.asJava))
    b.dbSubnetGroupName(subnetGroup.getName)
    b.dbClusterParameterGroupName(clusterParameterGroup.getName)
    engineMode.map(b.engineMode)
    scalingConfiguration.map(b.scalingConfiguration)
    serverlessv2ScalingConfiguration.map(b.serverlessv2ScalingConfiguration)
    snapshotIdentifier.map(b.snapshotIdentifier)
    masterUsername.map(b.masterUsername)
    masterPassword.map(b.masterPassword)
    b
  }

  def rdsClusterScalingConfiguration(
    minCapacity: Double,
    maxCapacity: Double,
    autoPause: Boolean,
    secondsUntilAutoPause: Int
  ) =
    RdsClusterScalingConfiguration
      .builder
      .autoPause(autoPause)
      .minCapacity(minCapacity)
      .maxCapacity(maxCapacity)
      .secondsUntilAutoPause(secondsUntilAutoPause)
      .build

  def rdsClusterServerlessv2ScalingConfiguration(
    minCapacity: Double,
    maxCapacity: Double
  ) =
    RdsClusterServerlessv2ScalingConfiguration
      .builder
      .minCapacity(minCapacity)
      .maxCapacity(maxCapacity)
      .build

  def addRdsClusterParameterGroup[D](
    name: String,
    family: String = "aurora-postgresql15",
    parameters: List[RdsClusterParameterGroupParameter] = List(
      RdsClusterParameterGroupParameter
        .builder
        .name("rds.babelfish_status")
        .value("on")
        .applyMethod("pending-reboot")
        .build,
      RdsClusterParameterGroupParameter
        .builder
        .name("babelfishpg_tds.tds_ssl_encrypt")
        .value("1")
        .build,
      RdsClusterParameterGroupParameter
        .builder
        .name("babelfishpg_tsql.migration_mode")
        .value("multi-db")
        .applyMethod("pending-reboot")
        .build
    )
  ) =
    buildAndAddResource[D, RdsClusterParameterGroup] { context =>
      RdsClusterParameterGroup.Builder
        .create(context.stack, s"$name-rds-cluster-parameter-group")
        .family(family)
        .parameter(parameters.asJava)
    }

  def addRdsClusterInstance[D](
    name: String,
    cluster: RdsCluster,
    instanceClass: String,
    publiclyAccessible: Boolean = false
  ) =
    buildAndAddResource[D, RdsClusterInstance] { context =>
      RdsClusterInstance.Builder
        .create(context.stack, s"$name-rds-cluster-instance")
        .identifier(name)
        .clusterIdentifier(cluster.getClusterIdentifier)
        .engine(cluster.getEngine)
        .engineVersion(cluster.getEngineVersion)
        .instanceClass(instanceClass)
        .publiclyAccessible(publiclyAccessible)
    }
}
