package org.ucf.ml

import java.util.concurrent.atomic.AtomicInteger
import org.ucf.ml.utils.{Common, Count}
import scala.collection.mutable
import com.github.javaparser.ast.Node
import scala.collection.mutable.HashMap
class Context(idioms:mutable.HashSet[String], granularity: Value = METHOD) extends Common {

  /**
   *
   */

  val positionalEmbedding = new HashMap[Node, PositionEmbeddingType]

  def getPositionalEmbedding(node:Node) = positionalEmbedding.get(node)

  def positionalEmbeddingIsContain(node: Node) = this.positionalEmbedding.contains(node)

  def addPositionalEmbedding(node: Node, pos:PositionEmbeddingType) =
    this.positionalEmbedding.+=(node -> pos)


  /*AST Tree Node Position*/
  private val position_offset = new AtomicInteger()
  def getNewPosition = position_offset.getAndIncrement()
  def getCurrentPositionOffset = position_offset

  // Current work mode
  private var current_mode = SOURCE
  def getCurrentMode = this.current_mode
  def setCurrentMode(target:Value) = {
    this.position_offset.set(0)
    this.current_mode = target
  }

  /*Data buffer to store parse results*/
  private val buggy_abstract = new StringBuilder()
  private val fixed_abstract = new StringBuilder()

  def get_abstract = this.getCurrentMode match {
    case SOURCE => get_buggy_abstract
    case TARGET => get_fixed_abstract
  }

  /**/
  private var isAddPostion = false
  def setPosition(value:Boolean) = this.isAddPostion = value

  def attachePosition(content:String) = if (isAddPostion) f"${content}#${this.getNewPosition} " else f"${content} "

  def append(content:String, numsIntent:Int = 0) = this.getCurrentMode match {
    case SOURCE => this.buggy_abstract.append(attachePosition(content))
    case TARGET => this.fixed_abstract.append(attachePosition(content))
  }

  private var isNewLine = false
  def setNewLine(value:Boolean) = this.isNewLine = value

  def appendNewLine(level:Int=0):Unit = this.getCurrentMode match {
    case SOURCE => if (isNewLine) this.buggy_abstract.append("\n")
    case TARGET => if (isNewLine) this.fixed_abstract.append("\n")
  }

  def get_buggy_abstract = buggy_abstract.toString()
  def get_fixed_abstract = fixed_abstract.toString()


  /* Generating abstrace code */
  var isAbstract = true
  def setIsAbstract(value:Boolean) = this.isAbstract = value


  private var gran = granularity
  def getGranularity = gran
  def setGranularity(value:Value) = this.gran = value
  ///////////////////////////////////////////////////////////////////////////////////////
  /********************* set up and look up statistical data ***************************/

  val ident_maps = new Count[String, String]("Ident", idioms)
  val textBlock_maps = new Count[String, String]("text", idioms)
  val string_maps = new Count[String, String]("String", idioms)
  val char_maps = new Count[String, String]("Char", idioms)
  val int_maps = new Count[String, String]("Integer", idioms)
  val float_maps = new Count[String, String]("Float", idioms)
  val long_maps = new Count[String, String]("Long", idioms)
  val double_maps = new Count[String, String]("Double", idioms)

  val type_maps = new Count[String, String]("Type", idioms)
  val method_maps = new Count[String, String]("Method", idioms)
  val variable_maps = new Count[String, String]("Varl", idioms)


  ///////////////////////////////////////////////////////////////////////////////////////
  /***************************** Helper functions *************************************/
  def dumpy_mapping(path:String=null) = {
    ident_maps.dump_data(path)
    textBlock_maps.dump_data(path)
    string_maps.dump_data(path)
    char_maps.dump_data(path)
    int_maps.dump_data(path)
    float_maps.dump_data(path)
    long_maps.dump_data(path)
    double_maps.dump_data(path)
    type_maps.dump_data(path)
    method_maps.dump_data(path)
    variable_maps.dump_data(path)
  }

  def clear = {

    this.position_offset.set(0)

    this.buggy_abstract.append ("\n")
    this.fixed_abstract.append ("\n")

    this.ident_maps.clear
    this.textBlock_maps.clear
    this.string_maps.clear
    this.char_maps.clear
    this.int_maps.clear
    this.float_maps.clear
    this.long_maps.clear
    this.double_maps.clear
    this.type_maps.clear
    this.method_maps.clear
    this.variable_maps.clear
  }

}
