package org.ucf.ml

import java.util.concurrent.atomic.AtomicInteger

import org.ucf.ml.utils.{Common, Count}

import scala.collection.mutable
import com.github.javaparser.ast.Node
import com.github.javaparser.ast.expr.SimpleName

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

    def get_token_abstract_with_position = {

      val length = token_position.toList.map(_.size).max
      (token_abstract.toList zip token_position.toList).map{case (token, position) => {
        val sparseList = length :: (for ((value, index) <- position.zipWithIndex if value !=0) yield index)
        s"${token}@[${sparseList.mkString(",")}]"
      }}.mkString(" ")
    }

    def printPretty() = {
      for ((token, position) <- token_abstract.toList zip token_position.toList) {
        // left alian 10 chars
        val sparseList = position.size :: (for ((value, index) <- position.zipWithIndex if value !=0) yield index)
        println("%-10s, [%d]-%s".format(token, position.size, sparseList))

      }
    }
  }

  private val buggy_abstract = AbstractContext(SOURCE)
  private val fixed_abstract = AbstractContext(TARGET)

  def get_buggy_abstract(isPosition:Boolean = false) =
    if (isPosition) buggy_abstract.get_token_abstract_with_position else buggy_abstract.get_token_abstract
  def get_fixed_abstract(isPosition:Boolean = false) =
    if (isPosition) fixed_abstract.get_token_abstract_with_position else fixed_abstract.get_token_abstract


  def buggy_toString = buggy_abstract.printPretty()
  def fixed_toString = fixed_abstract.printPretty()

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

  def get_abstract_code = this.getCurrentMode match {
    case SOURCE => this.get_buggy_abstract()
    case TARGET => this.get_fixed_abstract()
  }

  def append(content:String, numsIntent:Int = 0, position:PositionEmbeddingType = List.fill(1)(-1)) = this.getCurrentMode match {
    case SOURCE => {
      this.buggy_abstract.append_abstract(content)
      this.buggy_abstract.append_position(position)
    }

    case TARGET => {
      this.fixed_abstract.append_abstract(content)
      this.fixed_abstract.append_position(position)
    }
  }


  def appendPosition(content:String, index:Int=0, position:PositionEmbeddingType = null, parent:Node=null) = {

    val pos = if ((position == null) && (parent != null)) {
      val parant_pos = parent.genPosition(this)
      val pos_size = parent.getChildNodes.size() + nums_wrap_position
      val newIndex = if (index >= 0) index else (pos_size + index)
      val newNode = new SimpleName().setId(content).setParentNode(parent)
      val newposition = List.fill(pos_size)(0).updated(newIndex, 1) ::: parant_pos
      this.addPositionalEmbedding(newNode, newposition)
      newposition
    } else position

    if (pos != null)
      this.append(content = content, position = pos)
    else
      this.append(content = content)
  }


  private var isNewLine = false
  def setNewLine(value:Boolean) = this.isNewLine = value

  def appendNewLine(level:Int=0):Unit = this.getCurrentMode match {
    case SOURCE => if (isNewLine) this.buggy_abstract.append_abstract("\n")
    case TARGET => if (isNewLine) this.fixed_abstract.append_abstract("\n")
  }


  /* Generating abstract code */
  var isAbstract = true
  def setIsAbstract(value:Boolean) = this.isAbstract = value

  // CLASS or METHOD
  private var gran = granularity
  def getGranularity = gran
  def setGranularity(value:Value) = this.gran = value
  ///////////////////////////////////////////////////////////////////////////////////////
  /********************* set up and look up statistical data ***************************/

  // Java keywords and not appear identifiers in the context
  val ident_maps = new Count[String, String]("Ident", idioms)

  val textBlock_maps = new Count[String, String]("text", idioms)
  val string_maps = new Count[String, String]("String", idioms, true)
  val char_maps = new Count[String, String]("Char", idioms, true)
  val int_maps = new Count[String, String]("Integer", idioms, true)
  val float_maps = new Count[String, String]("Float", idioms, true)
  val long_maps = new Count[String, String]("Long", idioms, true)
  val double_maps = new Count[String, String]("Double", idioms, true)

  val type_maps = new Count[String, String]("Type", idioms, true)
  val method_maps = new Count[String, String]("Method", idioms, true)
  val variable_maps = new Count[String, String]("Var", idioms, true)


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
