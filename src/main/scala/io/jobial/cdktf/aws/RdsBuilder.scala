package io.jobial.cdktf.aws

import com.hashicorp.cdktf.providers.aws.rds_cluster.RdsCluster

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
