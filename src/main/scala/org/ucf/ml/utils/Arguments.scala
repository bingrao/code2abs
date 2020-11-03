package org.ucf.ml.utils

import net.sourceforge.argparse4j.ArgumentParsers
import net.sourceforge.argparse4j.inf.{ArgumentParser, ArgumentParserException, Namespace}

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.collection.JavaConversions._
import scala.reflect.io.File

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
    group.addArgument("-predt_path").help("The input path or directory for fixed code")
    group.addArgument("-n_best")
      .`type`(classOf[Int])
      .setDefault[Int](1).help("The number of n_best predictions")
  }

  def combine_abstraction(parser: ArgumentParser): Unit = {
    val group = parser.addArgumentGroup("combine")

    group.addArgument("-measure")
      .`type`(classOf[String])
      .setDefault[String]("ast")
      .help("So far only support ast diff compare")

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
    implicit class updateConfig(namespace: Namespace) {

      def update(key:String, value:Any): Unit = {
        if (namespace.getAttrs.containsKey(key)) {
          namespace.getAttrs.replace(key, value.asInstanceOf[Object])
        }
      }

      def update_json_Config(): Namespace = {
        implicit val formats = DefaultFormats
        val configPath = namespace.getString("config")
        val source = Source.fromFile(configPath).reader()
        val jObject = parse(source)

        for (elem <- namespace.getAttrs.keySet().toList) {
          (jObject \ elem) match {
            case value: JString => update(elem, value.extract[String])
            case value: JBool => update(elem, value.extract[Boolean])
            case value: JInt => update(elem, value.extract[Int])
            case value: JLong => update(elem, value.extract[Long])
            case value: JDouble => update(elem, value.extract[Double])
            case value: JArray =>
            case value: JDecimal =>
            case value: JSet =>
            case value: JObject =>
            case JNothing =>
            case _ =>
          }
        }
        namespace
      }

      def update_yml_config(): Namespace = {
        implicit val formats = DefaultFormats
        import org.yaml.snakeyaml.Yaml

        val configPath = namespace.getString("config")
        val source = Source.fromFile(configPath).reader()
        val ymal = new Yaml().load(source).asInstanceOf[java.util.LinkedHashMap[String, Any]]
        ymal.foreach{ case (key, value) => update(key, value)}
        namespace
      }
    }

    val parser = ArgumentParsers.newFor("Abstraction")
      .build().description("AST-Based Java Code abstration")

    parser.addArgument("-run_type")
      .choices("abstract", "astdiff", "combine", "gateway", "sequencer")
      .setDefault[String]("abstract")
      .required(false)
      .help("Spcify application type")

    parser.addArgument("-config")
      .`type`(classOf[String])
      .setDefault[String]("conf/template-app.yml")
      .help("The config path")

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
      val namespace = parser.parseArgs(args)

      val configPath = namespace.getString("config")
      if (File(configPath).exists) {
        if (configPath.endsWith("yml")) namespace.update_yml_config()
        if (configPath.endsWith("json")) namespace.update_json_Config()
      }
      namespace
    } catch {
      case e: ArgumentParserException => {
        parser.handleError(e)
        System.exit(-1)
      }
    }
    ns.asInstanceOf[Namespace]
  }
}
