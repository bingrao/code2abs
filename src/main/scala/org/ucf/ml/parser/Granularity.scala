package org.ucf.ml
package parser

import java.io.IOException
import java.nio.file.{Files, Paths}

abstract class Granularity extends utils.Common {
  protected def readSourceCode(input:String, isFile:Boolean = true) = {
    val reg:String = try {
      if(isFile) {
        new String(Files.readAllBytes(Paths.get(input)))
      } else {
        input
      }
    } catch {
      case e:IOException => {
        e.printStackTrace()
        EmptyString
      }
    }
    // Remove all comments and anotations in the source code
    reg.replaceAll("(?:/\\*(?:[^*]|(?:\\*+[^*/]))*\\*+/)|(?://.*)","") //remove comments
       .replaceAll("@.+", "") //remove annotations
  }

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