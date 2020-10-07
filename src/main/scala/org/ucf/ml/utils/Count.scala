package org.ucf.ml.utils

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Count[K, V <: String](name:String, idioms:mutable.HashSet[String],
                            exclude_keywords:Boolean=false, with_scope:Boolean=false) extends Common {

  private val data = collection.mutable.Map[K, ListBuffer[V]]()
  private val offset = new AtomicInteger(1)
  private val prefix = f"${name}_"
  private val keywords = List("abstract", "assert", "boolean", "break", "byte",
                              "case", "catch", "char", "class", "const",
                              "continue", "default", "do", "double", "else",
                              "enum", "extends", "final", "finally", "float",
                              "for", "goto", "if", "implements", "import",
                              "instanceof", "int", "interface", "long", "native",
                              "new", "package", "private", "protected", "public",
                              "return", "short", "static", "strictfp", "super",
                              "switch", "synchronized", "this", "throw", "throws",
                              "transient", "try", "void", "volatile", "while",
                              "true", "false", "null", "var", "const",
                              "goto")

  idioms.--=(keywords)


  def getPrefix = this.prefix
  private def getNewValue = (f"${name}_${this.getIncCount}").asInstanceOf[V]

  private def getIncCount = offset.getAndIncrement()
  private def getDecCount = offset.getAndDecrement()
  def getCurCount = offset

  def getKeys = data.keys.toList
  def getValues = data.map(_._2).toList

  def get(item:K) = data.get(item).get

  def add(key:K, value:V) = {
    data.+=(key -> ListBuffer[V](value))
  }

  def update(key:K, value:V) = {
    if (contain(key)) append(key, value) else add(key, value)
  }

  def append(key:K, value:V) = data(key).append(value)
  def remove(key:K) = data -= key

  def contain(key:K):Boolean = data.contains(key)
  def contain(key:K, scope:Int):Boolean = if (contain(key)) {
    val scopes = get(key).map(_.split("_").last.toInt)
    scopes.max <= scope
  } else false

  def getNewContent(key:K, code_scope:Int) = {

    def get_best_match(values:ListBuffer[V], scope:Int) = {
      val elements = values.map(v => {
        val diff_scope = scope - v.split("_").last.toInt
        (v, diff_scope)
      }).sortBy(_._2).head._1.split("_")


      if (elements(1).toInt == 0) elements(0) else elements.dropRight(1).mkString("_")
    }

    val reg = if (keywords.contains(key.toString) && ! exclude_keywords) {
      // the key is a keyword of java
      key.asInstanceOf[V]
    } else if (this.contain(key, code_scope)){
      // If there exists
      get_best_match(this.get(key), code_scope)
    } else if (idioms.contains(key.toString)) {
      // the key is a idiom
      val value = key.asInstanceOf[V]
      this.update(key, s"${value}_0_${code_scope}".asInstanceOf[V])
      value
    } else {
      val value = this.getNewValue
      this.update(key, s"${value}_${code_scope}".asInstanceOf[V])
      value
    }

    if (with_scope)
      s"${reg}[${code_scope}]"
    else
      reg
  }

  def dump_data(path:String=null) = if (!data.isEmpty) {
    val dump = data.toList.map{case (k,v) => f"${name}\t\t\t\t: ${k} -> [${v.mkString(",")}]"}.mkString("\n")

//    val dump = data.toList.sortBy(_._2.split("_").last.toInt)
//      .map{case (k,v) => f"${name}\t${v}\t${k}"}.mkString("\n")

    if (path != null)
      write(path, dump)
    else
      println(dump)
  }

  def clear = {
    this.data.clear()
    this.offset.set(0)
  }
}
