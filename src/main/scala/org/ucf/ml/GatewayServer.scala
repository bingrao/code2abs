package org.ucf.ml

import org.ucf.ml.parser.JavaParser
import py4j.GatewayServer

import scala.util.Random

object GatewayServer extends utils.Common {

  class ServerEntry {
    def output(data:String) = logger.info(data)
    def newList(nums:Int) = Seq.fill(nums)(Random.nextInt).toArray
  }


  def main(args: Array[String]): Unit = {
    val server = new GatewayServer(new JavaParser)
    server.start()
  }
}
