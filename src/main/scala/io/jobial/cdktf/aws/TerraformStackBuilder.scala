package io.jobial.cdktf.aws


trait TerraformStackBuilder extends IamBuilder with Ec2Builder
  with UserDataBuilder with EcsBuilder with CloudwatchBuilder
  with CdktfSupport