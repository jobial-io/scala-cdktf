package io.jobial.cdktf.aws

import com.hashicorp.cdktf.providers.aws.db_instance.DbInstance
import com.hashicorp.cdktf.providers.aws.db_subnet_group.DbSubnetGroup
import com.hashicorp.cdktf.providers.aws.rds_cluster.RdsCluster
import com.hashicorp.cdktf.providers.aws.rds_cluster_instance.RdsClusterInstance
import com.hashicorp.cdktf.providers.aws.rds_cluster_parameter_group.RdsClusterParameterGroup

import scala.collection.JavaConverters._

trait RdsBuilder extends IamBuilder {

  def addDbSubnetGroup[D](
    name: String
  ) =
    buildAndAddResource[D, DbSubnetGroup] { context =>
      DbSubnetGroup.Builder
        .create(context.stack, s"$name-db-subnet-group")
        .subnetIds(cbtechAllSubnets("dev").asJava)
    }


  def addRdsCluster1[D](
    name: String,
    engine: String,
    availabilityZones: List[String],
    masterUsername: String,
    masterPassword: String,
    subnetGroup: DbSubnetGroup,
    clusterParameterGroup: RdsClusterParameterGroup,
    vpcSecurityGroupIds: Option[List[String]] = None,
    databaseName: Option[String] = None,
    backupRetentionPeriod: Int = 5,
    preferredBackupWindow: String = "04:00-06:00",
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, RdsCluster]({ context =>
    val b = RdsCluster.Builder
      .create(context.stack, s"$name-rds-cluster")
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
    b.dbSubnetGroupName(subnetGroup.getName)
    b.dbClusterParameterGroupName(clusterParameterGroup.getName)
    b
  }, { (context, resource) =>
    //    resource.addOverride("babelfish", Map(
    //      "enabled" -> true,
    //      "version" -> "0.1.0"
    //    ).asJava)
    resource
  })

  def addRdsClusterParameterGroup[D](
    name: String
  ) =
    buildAndAddResource[D, RdsClusterParameterGroup] { context =>
      RdsClusterParameterGroup.Builder
        .create(context.stack, s"$name-rds-cluster-parameter-group")
        .family("aurora-postgresql15")
        .parameter(
          List(
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
              .build
          ).asJava
        )
    }

  def addRdsClusterInstance[D](
    name: String,
    cluster: RdsCluster
  ) =
    buildAndAddResource[D, RdsClusterInstance] { context =>
      RdsClusterInstance.Builder
        .create(context.stack, s"$name-rds-cluster-instance")
        .clusterIdentifier(cluster.getClusterIdentifier)
        .engine("aurora-postgresql")
        .instanceClass("db.r5.large")
        .publiclyAccessible(true)
    }

  def addDbInstance[D](
    name: String,
    cluster: RdsCluster
  ) =
    buildAndAddResource[D, DbInstance] { context =>
      DbInstance.Builder
        .create(context.stack, s"$name-rds-cluster-instance")
        //.(cluster.getClusterIdentifier)
        //        .engine("aurora-postgresql")
        //.instanceClass("db.r5.large")
        .publiclyAccessible(true)

    }
}
