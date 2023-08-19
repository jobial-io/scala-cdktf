package io.jobial.cdktf.aws

import com.hashicorp.cdktf.providers.aws.cloudwatch_log_group.CloudwatchLogGroup

trait CloudwatchBuilder extends TerraformStackBuilderCore {

  def addLogGroup[D](name: String, skipDestroy: Boolean = true) = buildAndAddResource[D, CloudwatchLogGroup] { context =>
    CloudwatchLogGroup.Builder
      .create(context.stack, s"$name-loggroup")
      .name(name)
      .skipDestroy(skipDestroy)
  }

}
