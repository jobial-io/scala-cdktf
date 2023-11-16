package io.jobial.cdktf.aws

import com.hashicorp.cdktf.providers.aws.cloudwatch_log_group.CloudwatchLogGroup
import io.jobial.cdktf.aws.TerraformStackBuildContext.NameTag

import scala.collection.JavaConverters._

trait CloudwatchBuilder extends TerraformStackBuilderCore {

  def addLogGroup[D](
    name: String,
    skipDestroy: Boolean = true,
    tags: Map[String, String] = Map()
  ) = buildAndAddResource[D, CloudwatchLogGroup] { context =>
    CloudwatchLogGroup.Builder
      .create(context.stack, s"$name-loggroup")
      .name(name)
      .skipDestroy(skipDestroy)
      .tags(context.mergeTags(name, tags).asJava)
  }

}
