package org.ucf.ml

import java.util.concurrent.atomic.AtomicInteger

import org.ucf.ml.utils.{Common, Count}

import scala.collection.mutable
import com.github.javaparser.ast.Node

import scala.collection.mutable.{HashMap, ListBuffer}
/**
 *  The Context object is shared by buggy and fixed partially regarding idioms and abstracts
 * @param idioms
 * @param granularity
 */
class Context(idioms:mutable.HashSet[String], granularity: Value = METHOD) extends Common {

  case class AbstractContext(target:Value) {
    private val token_abstract = new ListBuffer[String]
    private val token_position = new ListBuffer[PositionEmbeddingType]

    /*AST Tree Node Position*/
    private val token_offset = new AtomicInteger()

    def getNewPosition = token_offset.getAndIncrement()
    def getCurrentPositionOffset = token_offset

    def get_token_abstract = this.token_abstract.mkString(" ")
    def append_abstract(content:String) = this.token_abstract.+=(content)


    def get_token_poistion = this.token_position
    def append_position(pos:PositionEmbeddingType) = this.token_position.+=(pos)
  }

  private val buggy_abstract = AbstractContext(SOURCE)
  private val fixed_abstract = AbstractContext(TARGET)
  // Shared Object by fixed and buggy

  val positionalEmbedding = new HashMap[Node, PositionEmbeddingType]
  def getPositionalEmbedding(node:Node) = positionalEmbedding.get(node)

  def positionalEmbeddingIsContain(node: Node) = this.positionalEmbedding.contains(node)

  def addPositionalEmbedding(node: Node, pos:PositionEmbeddingType) =
    this.positionalEmbedding.+=(node -> pos)


  // Current work mode
  private var current_mode = SOURCE
  def getCurrentMode = this.current_mode
  def setCurrentMode(target:Value) = {
    this.current_mode = target
  }


  def getNewPosition = this.getCurrentMode match {
    case SOURCE => this.buggy_abstract.getNewPosition
    case TARGET => this.fixed_abstract.getNewPosition
  }

  def get_abstract = this.getCurrentMode match {
    case SOURCE => this.buggy_abstract.get_token_abstract
    case TARGET => this.fixed_abstract.get_token_abstract
  }

  def append(content:String, numsIntent:Int = 0, position:PositionEmbeddingType = null) = this.getCurrentMode match {
    case SOURCE => {
      this.buggy_abstract.append_abstract(content)
      this.buggy_abstract.append_position(position)
    }

    case TARGET => {
      this.fixed_abstract.append_abstract(content)
      this.fixed_abstract.append_position(position)
    }
  }

  private var isNewLine = false
  def setNewLine(value:Boolean) = this.isNewLine = value

  def appendNewLine(level:Int=0):Unit = this.getCurrentMode match {
    case SOURCE => if (isNewLine) this.buggy_abstract.append_abstract("\n")
    case TARGET => if (isNewLine) this.fixed_abstract.append_abstract("\n")
  }

  def get_buggy_abstract = buggy_abstract.get_token_abstract
  def get_fixed_abstract = fixed_abstract.get_token_abstract


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
}
