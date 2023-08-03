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
import software.amazon.jsii.Builder

import java.time.Instant.now
import scala.collection.JavaConverters._

case class TerraformStackBuildContext[D](
  name: String,
  stack: TerraformStack,
  app: App,
  data: D,
  containerDefinitions: Map[String, ContainerDefinition] = Map(),
  tags: Map[String, String] = Map()
) {
  def synth = IO(app.synth())

  def updateData(data: D) = copy(data = data)

  def addCointainerDefinition(containerDefinition: ContainerDefinition) =
    copy(containerDefinitions = containerDefinitions + (containerDefinition.name -> containerDefinition))

  def containerDefinitionsWithTransitiveDependencies(definitions: Iterable[ContainerDefinition]): List[ContainerDefinition] = {
    val names = definitions.map(_.name).toSet
    val closureNames = definitions.flatMap(_.dependsOn.map(_.containerName)).toSet ++ names
    val closure = containerDefinitions.values.filter(d => closureNames.contains(d.name)).toList
    if (names == closureNames)
      closure
    else
      containerDefinitionsWithTransitiveDependencies(closure)
  }

  def mergeTags(resourceName: Option[String], tags: Map[String, String]) =
    this.tags ++ tags ++
      (resourceName match {
        case Some(resourceName) =>
          Map(CdktfNameTag -> resourceName)
        case None =>
          Map()
      }) +
      (CdktfTimestampTag -> now.toString)

  def mergeTags(resourceName: String, tags: Map[String, String]): Map[String, String] =
    mergeTags(Some(resourceName), tags)

  def mergeTags(tags: Map[String, String]): Map[String, String] =
    mergeTags(None, tags)
}

object TerraformStackBuildContext {

  def apply[D](name: String, data: D, appContext: Map[String, _], tags: Map[String, String]) = {
    val config = AppConfig
      .builder
      .context(appContext.asJava)
      .build
    val app = new App(config)
    new TerraformStackBuildContext(name, new TerraformStack(app, name), app, data, tags = tags)
  }

  val CdktfNameTag = "cdktf:name"
  val CdktfTimestampTag = "cdktf:timestamp"
}

trait TerraformStackBuilder extends IamBuilder with Ec2Builder with UserDataBuilder with EcsBuilder with CloudwatchBuilder {

  type TerraformStackBuildState[D, A] = State[TerraformStackBuildContext[D], A]

  def createStack(name: String)(state: TerraformStackBuildState[Unit, Unit]) =
    createStack[Unit](name, ())(state)

  def createStack[D](name: String, data: D, appContext: Map[String, _] = defaultAppContext, tags: Map[String, String] = Map())(state: TerraformStackBuildState[D, Unit]) =
    state.run(TerraformStackBuildContext(name, data, appContext, tags)).value._1

  def defaultAppContext = Map(
    "excludeStackIdFromLogicalIds" -> true,
    "allowSepCharsInLogicalIds" -> true
  )

  def addResource[D, T](resource: T): TerraformStackBuildState[D, T] =
    State.inspect { context =>
      resource
    }

  def buildAndAddResource[D, T](builder: TerraformStackBuildContext[D] => Builder[T], postBuild: (TerraformStackBuildContext[D], T) => T = (_: TerraformStackBuildContext[D], resource: T) => resource): TerraformStackBuildState[D, T] =
    for {
      context <- getContext
      resource <- addResource(builder(context).build)
    } yield postBuild(context, resource)

  def addS3Backend[D](bucket: String, key: String) = buildAndAddResource[D, S3Backend] { context =>
    S3Backend.Builder
      .create(context.stack)
      .bucket(bucket)
      .key(key)
  }

  def addAwsProvider[D](region: String) = buildAndAddResource[D, AwsProvider] { context =>
    AwsProvider.Builder
      .create(context.stack, "AWS")
      .region(region)
  }

  def sequence[S, T](l: List[State[S, T]]): State[S, List[T]] =
    l.headOption match {
      case Some(h) =>
        for {
          h <- h
          t <- sequence(l.tail)
        } yield h :: t
      case None =>
        State.inspect[S, List[T]](_ => List())
    }

  def getContext[D]: TerraformStackBuildState[D, TerraformStackBuildContext[D]] = State.inspect { context =>
    context
  }

  def getData[D]: TerraformStackBuildState[D, D] =
    getContext.map(_.data)

  def updateData[D](f: D => D) =
    for {
      data <- getData[D]
      r <- State.modify[TerraformStackBuildContext[D]](_.updateData(f(data)))
    } yield r

  //
  //  def addMyResource[D](
  //    name: String
  //  ): TerraformStackBuildState[D, MyResource] =
  //    State.inspect { context =>
  //      new MyResource(context.stack)
  //    }

}

//class MyResource(scope: Construct) extends TerraformResource(software.amazon.jsii.JsiiObject.InitializationMode.JSII) {
//  software.amazon.jsii.JsiiEngine.getInstance.createNewObject(this, Array[AnyRef](java.util.Objects.requireNonNull(scope, "scope is required"), java.util.Objects.requireNonNull("hello", "id is required"), java.util.Objects.requireNonNull("", "config is required")))
//}
