package org.ucf.ml
package utils

import java.util.concurrent.atomic.AtomicInteger

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.expr.SimpleName

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
/**
 *  The Context object is shared by buggy and fixed partially regarding idioms and abstracts
 * @param idioms
 * @param granularity
 */
class Context(idioms:mutable.HashSet[String], granularity: Value = METHOD) extends Common {

//  import java.io._
//  val pw = new PrintWriter(new File("hello.txt" ))
//  idioms.foreach(ele => pw.write(ele+"\n"))
//  pw.close()

  case class AbstractContext(target:Value) {
    private val token_abstract = new ListBuffer[String]
    private val token_root_path = new ListBuffer[String]
    private val token_position = new ListBuffer[PositionEmbeddingType]

    /*AST Tree Node Position*/
    private val token_offset = new AtomicInteger()

    def getNewPosition: Int = token_offset.getAndIncrement()
    def getCurrentPositionOffset: AtomicInteger = token_offset

    def get_token_abstract: String = this.token_abstract.mkString(" ")
    def append_abstract(content:String):Unit = this.token_abstract.+=(content)

    def get_token_path: String = this.token_root_path.mkString(getRightDoubleArrow)
    def append_path(root_path:String):Unit = this.token_root_path.+=(root_path)


    def get_token_poistion: mutable.Seq[PositionEmbeddingType] = this.token_position
    def append_position(pos:PositionEmbeddingType): Unit = this.token_position.+=(pos)

    def get_token_abstract_with_position: String = {
      val length = token_position.toList.map(_.size).max
      (token_abstract.toList zip token_position.toList).map{case (token, position) =>
        val sparseList = length :: (for ((value, index) <- position.zipWithIndex if value !=0) yield index)
        s"$token@[${sparseList.mkString(",")}]"
      }.mkString(" ")
    }

    def printPretty():Unit = {
      for ((token, position) <- token_abstract.toList zip token_position.toList) {
        // left alian 10 chars
        val sparseList = position.size :: (for ((value, index) <- position.zipWithIndex if value !=0) yield index)
        println("%-10s, [%d]-%s".format(token, position.size, sparseList))
      }
    }
  }

  private val buggy_abstract: AbstractContext = AbstractContext(SOURCE)
  private val fixed_abstract: AbstractContext = AbstractContext(TARGET)

  def get_buggy_abstract(isPosition:Boolean = false): String =
    if (isPosition) buggy_abstract.get_token_abstract_with_position else buggy_abstract.get_token_abstract
  def get_fixed_abstract(isPosition:Boolean = false): String =
    if (isPosition) fixed_abstract.get_token_abstract_with_position else fixed_abstract.get_token_abstract

  def get_buggy_path(): String = buggy_abstract.get_token_path
  def get_fixed_path(): String = fixed_abstract.get_token_path


  def buggy_toString():Unit = buggy_abstract.printPretty()
  def fixed_toString():Unit = fixed_abstract.printPretty()

  // Shared Object by fixed and buggy

  val positionalEmbedding = new mutable.HashMap[Node, PositionEmbeddingType]
  def getPositionalEmbedding(node:Node): Option[PositionEmbeddingType] = positionalEmbedding.get(node)

  def positionalEmbeddingIsContain(node: Node): Boolean = this.positionalEmbedding.contains(node)

  def addPositionalEmbedding(node: Node, pos:PositionEmbeddingType): positionalEmbedding.type =
    this.positionalEmbedding.+=(node -> pos)


  // Current work mode
  private var current_mode = SOURCE
  def getCurrentMode: Value = this.current_mode
  def setCurrentMode(target:Value): Unit = {
    this.current_mode = target
  }


  def getNewPosition: Int = this.getCurrentMode match {
    case SOURCE => this.buggy_abstract.getNewPosition
    case TARGET => this.fixed_abstract.getNewPosition
  }

  def get_abstract_code: String = this.getCurrentMode match {
    case SOURCE => this.get_buggy_abstract()
    case TARGET => this.get_fixed_abstract()
  }

  def append(content:String,
             numsIntent:Int = 0,
             position:PositionEmbeddingType = List.fill(1)(-1),
             rootPath:String=""): Unit = {

    // Debug problem:  append token position by Bing 2021-03015
//    val newContent = s"${this.getNewPosition}@${content}"
    val newContent = content
    this.getCurrentMode match {
      case SOURCE =>
        this.buggy_abstract.append_abstract(newContent)
        this.buggy_abstract.append_position(position)
        this.buggy_abstract.append_path(rootPath)

      case TARGET =>
        this.fixed_abstract.append_abstract(newContent)
        this.fixed_abstract.append_position(position)
        this.fixed_abstract.append_path(rootPath)
    }
  }


  def appendPosition(content:String,
                     index:Int=0,
                     position:PositionEmbeddingType = null,
                     parent:Node=null,
                     rootPath: String=""): Unit = {

    val pos = if ((position == null) && (parent != null)) {
      val parant_pos = parent.genPosition(this)
      val pos_size = parent.getChildNodes.size() + nums_wrap_position
      val newIndex = if (index >= 0) index else pos_size + index
      val newNode = new SimpleName().setId(content).setParentNode(parent)
      val newposition = List.fill(pos_size)(0).updated(newIndex, 1) ::: parant_pos
      this.addPositionalEmbedding(newNode, newposition)
      newposition
    } else position

    if (pos != null)
      this.append(content = content, position = pos, rootPath = s"${content} @ ${rootPath}")
    else
      this.append(content = content, rootPath = s"${content} @ ${rootPath}")
  }


  private var isNewLine = false
  def setNewLine(value:Boolean): Unit = this.isNewLine = value

  def appendNewLine(level:Int=0):Unit = this.getCurrentMode match {
    case SOURCE => if (isNewLine) this.buggy_abstract.append_abstract("\n")
    case TARGET => if (isNewLine) this.fixed_abstract.append_abstract("\n")
  }


  val with_scope:Boolean = false

  /* Generating abstract code */
  var isAbstract = true
  def setIsAbstract(value:Boolean): Unit = this.isAbstract = value

  // CLASS or METHOD
  private var gran = granularity
  def getGranularity: Value = gran
  def setGranularity(value:Value): Unit = this.gran = value
  ///////////////////////////////////////////////////////////////////////////////////////
  /********************* set up and look up statistical data ***************************/
  val bpe_enable = true
  val bpe_map = new Count[String, CountEntry]("BPE", idioms, with_scope=with_scope)



  // Java keywords and not appear identifiers in the context
  val ident_maps = new Count[String, CountEntry]("IDENT", idioms, with_scope=with_scope)

  val textBlock_maps = new Count[String, CountEntry]("TEXT", idioms, with_scope=with_scope)
  val string_maps = new Count[String, CountEntry]("STRING", idioms, true, with_scope=with_scope)
  val char_maps = new Count[String, CountEntry]("CHAR", idioms, true, with_scope=with_scope)
  val int_maps = new Count[String, CountEntry]("INT", idioms, true, with_scope=with_scope)
  val float_maps = new Count[String, CountEntry]("FLOAT", idioms, true, with_scope=with_scope)
  val long_maps = new Count[String, CountEntry]("LONG", idioms, true, with_scope=with_scope)
  val double_maps = new Count[String, CountEntry]("DOUBEL", idioms, true, with_scope=with_scope)

  val type_maps = new Count[String, CountEntry]("TYPE", idioms, false, with_scope=with_scope)
  val method_maps = new Count[String, CountEntry]("METHOD", idioms, true, with_scope=with_scope)
  val variable_maps = new Count[String, CountEntry]("VAR", idioms, true, with_scope=with_scope)


  ///////////////////////////////////////////////////////////////////////////////////////
  /***************************** Helper functions *************************************/
  def dumpy_mapping(path:String=null): Unit = {
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
    bpe_map.dump_data(path)
  }
}
