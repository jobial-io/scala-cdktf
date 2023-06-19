package io.jobial.cdktf.util.json

import io.circe.Encoder
import io.circe.syntax.EncoderOps

trait JsonImplicits {
  
  implicit def toJson[T: Encoder](value: T) = value.asJson
}
