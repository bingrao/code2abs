package org.ucf

import scala.collection.mutable

package object ml extends Enumeration {

  // recursive function return value for gencode func in implicit class
  final val EmptyString:String = ""

  /*Positional Embedding type for a AST node*/
  type PositionEmbeddingType = List[Double]

  // input file Granularity
  val CLASS = Value("class")
  val METHOD = Value("method")


  // Working Mode
  val SOURCE = Value("buggy") // default model
  val TARGET = Value("fixed")

  @deprecated
  val os = System.getProperty("os.name").toLowerCase
  @deprecated
  def updatePathFormat(path:String):String = if (os.contains("linux"))
    path.replace("\\", "/")
  else if (os.contains("windows"))
    path.replace("\\", "/")
  else path


  // add log functions to all Scala Objects
  @deprecated
  implicit class AddUtils(any:AnyRef) {
    //https://alvinalexander.com/scala/scala-functions-repeat-character-n-times-padding-blanks
    def getIndent(nums:Int) = "\t" * nums
    def printPretty(sb:mutable.StringBuilder, numsIntent:Int) = sb.append("")
  }

  // https://en.wikipedia.org/wiki/List_of_Unicode_characters#Arrows
  def getUpArrow = " " + '\u2191' + " "
  def getDownArrow = " " + '\u2193' + " "
}

