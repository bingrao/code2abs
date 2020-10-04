package org.ucf.ml
import parser.JavaParser


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

object GatewayServer extends utils.Common {
  def main(args: Array[String]): Unit = {

    val port_nums = if (args.size == 1) args(0).toInt else 25333

    val server = new py4j.GatewayServer(new JavaParser, port_nums)
    server.start()
    logger.info(s"Start Py4J Server [${port_nums}] to recieve python call ...")
  }
}
