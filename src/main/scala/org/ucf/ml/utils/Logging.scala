package org.ucf.ml.utils
import  org.apache.log4j.Logger

class Logging(name:String){
  private val logger = Logger.getLogger(name)

  def info(message:String,prefix:Boolean = true) =
    if (prefix) logger.info(message) else println(message)
  def warn(message:String,prefix:Boolean = true) =
    if (prefix) logger.warn(message) else println(message)
  def debug(message:String,prefix:Boolean = true) =
    if (prefix) logger.debug(message) else println(message)
  def error(message:String,prefix:Boolean = true) =
    if (prefix) logger.debug(message) else println(message)

  def isDebugEnabled = logger.isDebugEnabled

  def updateFileAppender(log_file:String) = {
    import org.apache.log4j.RollingFileAppender
    import org.apache.log4j.PatternLayout
    val layout = new PatternLayout("%d{ISO8601} [%t] %-5p %c %x - %m%n")
    try {
      val fileAppender = new RollingFileAppender(layout, log_file, true)
      this.logger.addAppender(fileAppender)
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}