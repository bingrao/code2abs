package org.ucf.ml
import parser.JavaParser

object GatewayServer extends utils.Common {
  def main(args: Array[String]): Unit = {
    val server = new py4j.GatewayServer(new JavaParser, 18888)
    server.start()
    logger.info("Start Py4J Server to recieve python call ...")
  }
}
