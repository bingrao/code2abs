package org.ucf.ml

import parser.JavaParser
import net.sourceforge.argparse4j.inf.{Namespace => ConfigNamespace}
import scala.collection.JavaConversions._

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

class Runner extends utils.Arguments {
  def gateway_server(config: ConfigNamespace) = {

    val port_num = config.getInt("port_num")

    val server = new py4j.GatewayServer(new JavaParser, port_num)
    server.start()
    logger.info(s"Start Py4J Server [${port_num}] to recieve python call ...")
  }

  def gen_sequencer_data (config: ConfigNamespace) = {

    val src_path = config.getString("buggy_path")
    val tgt_path = config.getString("fixed_path")
    val output_dir = config.getString("output_dir")
    val idioms_path = config.getString("idioms_path")
    val max_length = config.getInt("max_length")

    logger.info("Generate SequenceR datasets ...")
    val javaPaser = new parser.JavaParser
    javaPaser.genSequencerData(src_path, tgt_path, output_dir, idioms_path, max_length)
  }

  def gen_ast_diff(config: ConfigNamespace) = {
    val src = config.getString("buggy_path")
    val tgt = config.getString("fixed_path")
    val javaPaser = new parser.JavaParser
    javaPaser.getASTDiffCount(src, tgt, granularity = METHOD, isFile = false)
  }

  def combine_java_files(config: ConfigNamespace) = {
    val bugggy_dir = config.getString("buggy_path")
    val fixed_dir = config.getString("fixed_path")
    val output = config.getString("output_dir")

    val javaPaser = new parser.JavaParser

//    javaPaser.genCombinedFiles(bugggy_dir, fixed_dir, output)
  }

  def gen_abstract_code(config: ConfigNamespace) = {
    val config_path = config.getString("config")
    val worker = new parallel.Master(config)
    worker.run()
  }

}


object Runner extends Runner {

  def main(args: Array[String]): Unit = {
    val config = getArguments(args)

    config.getAttrs.foreach{case (key, value) =>
      logger.info(s"${key} -> ${value}")
    }


    config.getString("run_type") match {
      case "abstract" => gen_abstract_code(config)
      case "astdiff" => gen_ast_diff(config)
      case "sequencer" => gen_sequencer_data(config)
      case "combine" => combine_java_files(config)
      case "gateway" => gateway_server(config)
      case _ =>
        logger.info(s"The input does not match: ${args}")
    }

  }
}