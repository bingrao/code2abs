package org.ucf.ml

/**
  * @author Bing
  * How to run jar file
 *  1. Using Scala command
 * # export JAVA_OPTS="-Xmx32G -Xms1g -Xss512M -Dlog4j.configuration=file:///${ConfigPath}/log4j.properties"
 * # scala "${BinPath}"/java_abstract-1.0-jar-with-dependencies.jar "abstract" "${ConfigAbstract}"
 *
 *  2. Using Java command
 * # export JAVA_OPTS="-Xmx32G -Xms1g -Xss512M -Dlog4j.configuration=file:///${ConfigPath}/log4j.properties"
 * # java "${JAVA_OPTS}" -cp "${BinPath}"/java_abstract-1.0-jar-with-dependencies.jar org.ucf.ml.App "abstract" "${ConfigAbstract}"
  */
object App extends utils.Common {
  def main(args: Array[String]): Unit = {

    if (args.size == 0) {
      logger.error("Please specify configuration path ...")
      System.exit(-1)
    }

    val exit_code = args(0) match {
      case "abstract" => {
        val config_path = args(1)
        assert(args.size == 2)
        val worker = new parallel.Master(config_path)
        worker.run()
        1
      }
      case "astdiff" => {
        assert(args.size == 3)
        val src = args(1)
        val tgt = args(2)
        val javaPaser = new parser.JavaParser
        javaPaser.getASTDiffCount(src, tgt, granularity = METHOD, isFile = false)
      }
    }
    System.exit(exit_code)
  }
}