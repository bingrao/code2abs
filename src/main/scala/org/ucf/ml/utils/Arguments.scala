package org.ucf.ml.utils

import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

class Arguments extends Common {
  def config_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("abstract")
    group.addArgument("-with_position")
      .`type`(classOf[Boolean])
      .setDefault[Boolean](false)
      .help("Generate abstract code with position information")

    group.addArgument("-output_position")
      .`type`(classOf[Boolean])
      .setDefault[Boolean](false)
      .help("Generate abstract code with position information and output position as well")
    group.addArgument("-split_data")
      .`type`(classOf[Boolean])
      .setDefault[Boolean](false)
      .help("Split data into train, test and valid dataset with ratio of 8:1:1")
  }

  def ast_diff_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("astdiff")
  }

  def combine_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("combine")

  }

  def gateway_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("gateway")
    group.addArgument("-port_num")
      .`type`(classOf[Long])
      .setDefault[Long](25333)
      .help("The network port number for Py4j Server {be default: 25333}")
  }

  def sequencer_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("sequencer")
    group.addArgument("-max_length", "--max_length")
      .setDefault[Long](24000)
      .help("The max length of input token will be parsed")

  }

  def getArguments(args: Array[String]): Namespace = {

    val parser = ArgumentParsers.newFor("Abstraction")
      .build().description("AST-Based Java Code abstration")

    parser.addArgument("-run_type")
      .choices("abstract", "astdiff", "combine", "gateway", "sequencer")
      .setDefault[String]("abstract")
      .help("Spcify application type")

    parser.addArgument("-config").help("The config path")

    parser.addArgument("-buggy_path").help("The input path or directory for buggy code")
    parser.addArgument("-fixed_path").help("The input path or directory for fixed code")

    parser.addArgument("-output_dir").help("The input directory for output")

    parser.addArgument("-idioms_path").help("The idioms tokens path")

    parser.addArgument("-nums_worker")
      .`type`(classOf[Int])
      .setDefault[Int](1).help("The number of workers run the job in parallel")

    combine_abstraction(parser)

    ast_diff_abstraction(parser)

    config_abstraction(parser)

    gateway_abstraction(parser)

    sequencer_abstraction(parser)

    val ns = try {
      parser.parseArgs(args)
    } catch {
      case e: ArgumentParserException => {
        parser.handleError(e)
        System.exit(-1)
      }
    }
    ns.asInstanceOf[Namespace]
  }
}
