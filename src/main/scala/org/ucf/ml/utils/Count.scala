package org.ucf.ml
package utils

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class CountEntry(key:String, scope:Int, mode: Value, path:String) {
  def getKey = key
  def getScope = scope
  def getPath = path
  def getMode = mode

  def getMaxMatchPath(tgt:String) = {
    val srcPath = getPath.split(getUpArrow)
    val tgtPath = tgt.split(getUpArrow)
    srcPath.zip(tgtPath).count{case (key1, key2) => key1 == key2}
  }

  override def toString: String = s"\n\t\t${mode}_${key}_${scope}_${path}"
}

class Count[K, V <: CountEntry](name:String, idioms:mutable.HashSet[String],
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


  def getIdioms = this.idioms
  def getKeywords = this.keywords
  def getData = this.data


  def getPrefix = this.prefix
  private def getNewValue = f"${name}${this.getIncCount}"

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
//  def contain(key:K, scope:Int, mode: Value, path:String="NoPath"):Boolean = if (contain(key)) {
////    val scopes = get(key).map(_.split("_")(1).toInt)
////    scopes.max <= scope
//    true
//  } else false

//  def get_match_content(key:K, scope:Int, isNew:Boolean=false) = {
//    val reg = if (isNew) {
//      val value = this.getNewValue
//      this.update(key, s"${value}_${scope}".asInstanceOf[V])
//      value
//    } else {
//      val values = get(key)
//      val elements = values.map( v => {
//        val diff_scope = scope - v.split("_").last.toInt
//        (v, diff_scope)
//      }).sortBy(_._2).head._1.split("_")
//
//      if (!name.toLowerCase.contains("var")) {
//        val update_value = s"${elements.dropRight(1).mkString("_")}_${scope}".asInstanceOf[V]
//        if (!values.contains(update_value))
//          this.update(key, update_value)
//      }
//
//      if (elements(1).toInt == 0)
//        elements(0)
//      else
//        elements.dropRight(1).mkString("_")
//    }
//
//    if (with_scope)
//      s"${reg}[${scope}]"
//    else
//      reg
//  }


  def get_match_content(key:K, scope:Int, mode: Value, path:String = "NoPath") = {

    val lastPath = path.split(getUpArrow).last
    val isDeclare = "VariableDeclarator" == lastPath || "Parameter" == lastPath
    if (isDeclare) {
      // This item is a new declare varaible
      val newKey = if (mode == TARGET) {
        val declareValuesSource = get(key).filter(value => {
          (value.getMode == SOURCE) && (value.getPath == path)
        })
        if (declareValuesSource.nonEmpty) declareValuesSource.last.getKey else this.getNewValue
      } else {
        this.getNewValue
      }
      val value = CountEntry(newKey, scope, mode, path).asInstanceOf[V]
      this.update(key, value)
      value.getKey
    } else if ("NameExpr" == lastPath) {
      /**
       * There are two cases
       * 1. this item has been defined in context as a variable
       * 2. this item just has been referred, but not be defined
       */
      val declareValues = get(key).filter(value => {
        val lastPath = value.getPath.split(getUpArrow).last
        (value.getMode == mode) && ("VariableDeclarator" == lastPath || "Parameter" == lastPath)
      })

      val reachmatch = if (declareValues.nonEmpty) {
        // Search best reach case

        val matchcount = declareValues.map(v => (v, v.getMaxMatchPath(path))).maxBy(_._2)

        matchcount._1
      } else get(key).head

      val value = CountEntry(reachmatch.getKey, scope, mode, path).asInstanceOf[V]
      this.update(key, value)
      reachmatch.getKey
    } else {
      val reachmatch = get(key).head
      val value = CountEntry(reachmatch.getKey, scope, mode, path).asInstanceOf[V]
      this.update(key, value)
      reachmatch.getKey
    }







//    val elements = values.map( v => {
//      val diff_scope = scope - v.getScope
//      (v, diff_scope)
//    }).sortBy(_._2).head._1.split("_").dropRight(1)
//
//
//    if (!name.toLowerCase.contains("var")) {
//      val update_value = s"${elements.dropRight(1).mkString}_${scope}_${path}".asInstanceOf[V]
//      if (!values.contains(update_value))
//        this.update(key, update_value)
//    }
//
//    val reg = if (elements(1).toInt == 0)
//      elements(0)
//    else
//      elements.dropRight(1).mkString("_")
//
//    if (with_scope)
//      s"${reg}[${scope}]"
//    else
//      reg
  }

  def getNewContent(key:K, code_scope:Int, mode: Value, path:String = "NoPath") = {
    val reg = if (keywords.contains(key.toString) && ! exclude_keywords) {
      // the key is a keyword of java
      key.asInstanceOf[String]
    } else if (this.contain(key)) {
      // If there exists
      // 1. if it is a new variable declare, then create a new variable name
      // 2. otherwise, retrieve the best match from the list
      get_match_content(key, code_scope, mode, path)
    } else if (idioms.contains(key.toString)) {
      // the key is a idiom
      val value = CountEntry(key.asInstanceOf[String], code_scope, mode, path).asInstanceOf[V]
      this.update(key, value)
      value.getKey
    } else {
      val value = CountEntry(this.getNewValue, code_scope, mode, path).asInstanceOf[V]
      this.update(key, value)
      value.getKey
    }
    if (with_scope)
      s"${reg}[${code_scope}]"
    else
      reg
  }

  def dump_data(path:String=null) = if (!data.isEmpty) {
    val dump = data.toList.map{case (k,v) => f"${name}\t\t\t\t: ${k} -> [${v.mkString(",")}]"}.mkString("\n")

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
