package org.ucf.ml

/**
  * @author 
  */
object App extends utils.Common {
  def main(args: Array[String]): Unit = {

    if (args.size == 0) {
      logger.error("Please specify configuration path ...")
      System.exit(-1)
    }

    val config_path = args(0)

    val worker = new parallel.Master(config_path)
    worker.run()
  }
}