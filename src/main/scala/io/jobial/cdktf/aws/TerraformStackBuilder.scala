package io.jobial.cdktf.aws

import cats.data.State
import cats.effect.IO
import com.hashicorp.cdktf.App
import com.hashicorp.cdktf.AppConfig
import com.hashicorp.cdktf.S3Backend
import com.hashicorp.cdktf.TerraformStack
import com.hashicorp.cdktf.providers.aws.provider.AwsProvider
import io.jobial.cdktf.aws.TerraformStackBuildContext.CdktfNameTag
import io.jobial.cdktf.aws.TerraformStackBuildContext.CdktfTimestampTag
import io.jobial.cdktf.aws.TerraformStackBuildContext.NameTag
import software.amazon.jsii.Builder

import java.time.Instant.now
import scala.collection.JavaConverters._


trait TerraformStackBuilder extends IamBuilder with Ec2Builder
  with UserDataBuilder with EcsBuilder with CloudwatchBuilder
  with CdktfSupport