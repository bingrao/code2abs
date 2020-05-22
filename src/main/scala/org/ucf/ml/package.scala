package org.ucf

import com.github.javaparser.ast.Node

import scala.collection.mutable

package object ml extends Enumeration {

  val nums_wrap_position = 6

  implicit class genPosition(node:Node) {
    def genPositionalEmbedding(ctx:Context):PositionEmbeddingType = {
      if (ctx.positionalEmbeddingIsContain(node))
        ctx.getPositionalEmbedding(node).get
      else if (node.getParentNode.isPresent) {
        val pararent = node.getParentNode.get()
        val branch = pararent.getChildNodes.indexOf(node)
        val position = List.fill(pararent.getChildNodes.size() + nums_wrap_position)(0.0)
          .updated(branch + nums_wrap_position/2, 1.0)

        val pararent_position = if (ctx.positionalEmbeddingIsContain(pararent))
          ctx.getPositionalEmbedding(pararent).get
        else {
          val par_pos = pararent.genPositionalEmbedding(ctx)
          ctx.addPositionalEmbedding(pararent, par_pos)
          par_pos
        }
        val new_position = position ::: pararent_position
        ctx.addPositionalEmbedding(node, new_position)
        new_position
      } else {
        List.fill(node.getChildNodes.size())(0.0)
      }
    }
  }

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

