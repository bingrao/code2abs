package org.ucf.ml

import parser.JavaParser
import net.sourceforge.argparse4j.inf.{Namespace => ConfigNamespace}
import org.ucf.ml.parallel.ASTDiffMaster
import org.ucf.ml.utils.Vocabulary

import scala.collection.JavaConversions._

/**
 * @author Bing
 *         How to run jar file
 *  1. Using Scala command
 * # export JAVA_OPTS="-Xmx32G -Xms1g -Xss512M -Dlog4j.configuration=file:///${ConfigPath}/log4j.properties"
 * # scala "${BinPath}"/java_abstract-1.0-jar-with-dependencies.jar -config "${ConfigFile}" | tee -a "${LogFile}"
 *
 *  2. Using Java command
 * # scala "${BinPath}"/java_abstract-1.0-jar-with-dependencies.jar -run_type "abstract" \
 *         -buggy_path "examples/learning_fix/data/${dataset}/raw/buggy/" \
 *         -fixed_path "examples/learning_fix/data/${dataset}/raw/fixed/" \
 *         -output_dir "examples/learning_fix/data/${dataset}/" \
 *         -idioms_path "examples/learning_fix/data/idioms/idioms.csv" \
 *         -nums_worker 10 \
 *         -with_position false \
 *         -output_position false | tee -a "${LogFile}"
 */

class Runner extends utils.Arguments {
  def run_gateway(config: ConfigNamespace) = {

    val port_num = config.getInt("port_num")

    val server = new py4j.GatewayServer(new JavaParser, port_num)
    server.start()
    logger.info(s"Start Py4J Server [${port_num}] to recieve python call ...")
  }

  def run_sequencer (config: ConfigNamespace) = {

    val src_path = config.getString("buggy_path")
    val tgt_path = config.getString("fixed_path")
    val output_dir = config.getString("output_dir")
    val idioms_path = config.getString("idioms_path")
    val max_length = config.getInt("max_length")

    logger.info("Generate SequenceR datasets ...")
    val javaPaser = new parser.JavaParser
    javaPaser.genSequencerData(src_path, tgt_path, output_dir, idioms_path, max_length)
  }

  def run_astdiff(config: ConfigNamespace) = {
    /**
     * scala "${BinPath}"/java_abstract-1.0-jar-with-dependencies.jar -run_type "astdiff" \
     *  -fixed_path "${FIXED_PATH}" \
     *  -predt_path "${PREDT_PATH}" \
     *  -n_best "${n_best}" \
     *  -nums_worker "${nums_worker}" \
     *  -output_dir "${OUTPUT_DIR}" | tee -a "${LogFile}"
     */
    val worker = new ASTDiffMaster(config)
    worker.run()
  }

  def run_combine(config: ConfigNamespace) = {
    val bugggy_dir = config.getString("buggy_path")
    val fixed_dir = config.getString("fixed_path")
    val output = config.getString("output_dir")

    val javaPaser = new parser.JavaParser

    javaPaser.genCombinedFiles(bugggy_dir, fixed_dir, output)
  }

  def run_abstraction(config: ConfigNamespace) = {
    /**
     * #  scala "${BinPath}"/java_abstract-1.0-jar-with-dependencies.jar -run_type "abstract" \
     * #        -buggy_path "examples/learning_fix/data/${dataset}/raw/buggy/" \
     * #        -fixed_path "examples/learning_fix/data/${dataset}/raw/fixed/" \
     * #        -output_dir "examples/learning_fix/data/${dataset}/" \
     * #        -idioms_path "examples/learning_fix/data/idioms/idioms.csv" \
     * #        -nums_worker 10 \
     * #        -with_position false \
     * #        -output_position false | tee -a "${LogFile}"
     */
    val worker = new parallel.AbstractMaster(config)
    worker.run()
  }

  def run_vocabulary(config: ConfigNamespace):Unit = {
    val vocab = new Vocabulary(config)
    vocab.run()
  }

}


object Runner extends Runner {

  def main(args: Array[String]): Unit = {
    val config = getArguments(args)

    config.getAttrs.foreach{case (key, value) =>
      logger.info(s"${key} -> ${value}")
    }

    logger.updateFileAppender(config.getString("log_file"))

    config.getString("run_type") match {
      case "abstract" => run_abstraction(config)
      case "astdiff" => run_astdiff(config)
      case "sequencer" => run_sequencer(config)
      case "combine" => run_combine(config)
      case "gateway" => run_gateway(config)
      case "vocabulary" => run_vocabulary(config)
      case _ =>
        logger.info(s"The input does not match: ${args}")
    }

  }
}