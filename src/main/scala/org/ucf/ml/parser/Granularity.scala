package org.ucf.ml
package parser

abstract class Granularity extends utils.Common {
  def getSourceCode(input:String=null):String
}

case class Granularity_Class(sourcePath:String, isFile:Boolean = true) extends Granularity {
  override def getSourceCode(input: String=sourcePath): String = readSourceCode(input, isFile)
}

case class Granularity_Method(sourcePath:String, isFile:Boolean = true) extends Granularity {
  override def getSourceCode(input: String=sourcePath): String =
    raw"public class DummyClass { ${readSourceCode(input, isFile)} }"
}


object Granularity {
  def apply(sourcePath:String, granularity: Value, isFile:Boolean = true): Granularity = granularity match {
    case CLASS => Granularity_Class(sourcePath, isFile)
    case METHOD => Granularity_Method(sourcePath, isFile)
    case _ => Granularity_Class(sourcePath, isFile)
  }
}