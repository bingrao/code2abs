package org.ucf.ml.utils

import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

class Arguments extends Common {
  def config_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("abstract")
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
      .help("The network port number for Py4j Server {be default: 25333}")
  }

  def sequencer_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("sequencer")
    group.addArgument("-max_length", "--max_length").help("The max length of input token will be parsed")

  }

  def getArguments(args: Array[String]): Namespace = {

    val parser = ArgumentParsers.newFor("Abstraction")
      .build().description("AST-Based Java Code abstration")

    parser.addArgument("-run_type")
      .choices("abstract", "astdiff", "combine", "gateway", "sequencer")
      .help("Spcify application type")

    parser.addArgument("-config").help("The config path")

    parser.addArgument("-buggy_path").help("The input path or directory for buggy code")
    parser.addArgument("-fixed_path").help("The input path or directory for fixed code")

    parser.addArgument("-output_dir").help("The input directory for output")

    parser.addArgument("-idioms_path").help("The idioms tokens path")


    combine_abstraction(parser)

    ast_diff_abstraction(parser)

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
