package io.jobial.cdktf.aws


trait TerraformStackBuilder extends TerraformStackBuilderCore
  with IamBuilder with Ec2Builder
  with UserDataBuilder with EcsBuilder with CloudwatchBuilder
  with RdsBuilder with CdktfSupport with DeploymentSupport