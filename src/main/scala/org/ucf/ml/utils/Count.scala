package org.ucf.ml.utils

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable

class Count[K, V <: String](name:String, idioms:mutable.HashSet[String]) extends Common {

  private val data = collection.mutable.Map[K, V]()
  private val offset = new AtomicInteger(0)
  private val prefix = f"${name}_"

  def getPrefix = this.prefix
  private def getNewValue = (f"${name}_${this.getIncCount}").asInstanceOf[V]

  private def getIncCount = offset.getAndIncrement()
  private def getDecCount = offset.getAndDecrement()
  def getCurCount = offset

  def getKeys = data.keys.toList
  def getValues = data.map(_._2).toList

  def get(item:K) = data.get(item)

  def add(key:K, value:V) = {
    data.+=(key -> value)
  }
  def update(key:K, value:V) = data(key) = value
  def remove(key:K) = data -= key
  def contain(key:K) = data.contains(key)


  def getNewContent(key:K) = {
    if (idioms.contains(key.toString))
      // the key is a idiom
      key.asInstanceOf[V]
    else if (this.contain(key)){
      // If there exists
      this.get(key).get
    } else {
      val value = this.getNewValue
      this.add(key, value)
      value
    }
  }

  def dump_data(path:String=null) = if (!data.isEmpty) {
    val dump = data.toList.sortBy(_._2.split("_").last.toInt)
      .map{case (k,v) => f"${name}\t${v}\t${k}"}.mkString("\n")
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
