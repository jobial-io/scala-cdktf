package io.jobial

package object cdktf {

  type MainClass = {def main(args: Array[String])}

  def normalizedClassName[C <: MainClass](app: C): String =
    normalizedClassName(app.getClass)

  def normalizedClassName(app: Class[_]): String =
    normalizedClassName(app.getName)

  def normalizedClassName(className: String): String =
    className.replaceAll("\\$$", "")
}
