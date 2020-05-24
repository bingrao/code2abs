package org.ucf.ml
package utils

import java.io.File
import com.typesafe.config.ConfigFactory

class PropertiesLoader(configPath:String = "src/main/resources/default_application.conf") {

  val parsedConfig = ConfigFactory.parseFile(new File(configPath))

  private val conf = ConfigFactory.load(parsedConfig)

  val getLogLevel = conf.getString("LogLevel")


  def getIdiomsPath = conf.getString("IdiomsPath")
  def getRawBuggyFilesDir = conf.getString("RawBuggyFilesDir")
  def getRawFixedFilesDir = conf.getString("RawFixedFilesDir")

  def getOutputBuggyDir = conf.getString("OutputBuggyDir")
  def getOutputFixedDir = conf.getString("OutputFixedDir")

  def getNumsWorker = conf.getInt("NumsWorker")

  def getIsParallel = conf.getBoolean("IsParallel")

  def getIsSplitData = conf.getBoolean("IsSplitData")

  def getIsWithPosition = conf.getBoolean("IsWithPosition")

  def getIsConcatPosition = conf.getBoolean("IsConcatPosition")

}