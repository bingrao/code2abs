package org.ucf.ml
package utils
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.printer.PrettyPrinterConfiguration
import parser.Granularity

import scala.collection.parallel._
import net.sourceforge.argparse4j.inf.{Namespace => ConfigNamespace}

import scala.collection.JavaConversions._

class Vocabulary(config: ConfigNamespace) extends parser.JavaParser {


  private val keywords = List("abstract", "assert", "boolean", "break", "byte",
    "case", "catch", "char", "class", "const",
    "continue", "default", "do", "double", "else",
    "enum", "extends", "final", "finally", "float",
    "for", "goto", "if", "implements", "import",
    "instanceof", "int", "interface", "long", "native",
    "new", "package", "private", "protected", "public",
    "return", "short", "static", "strictfp", "super",
    "switch", "synchronized", "this", "throw", "throws",
    "transient", "try", "void", "volatile", "while",
    "true", "false", "null", "var", "const",
    "goto")

  def get_statistics(data:ParSeq[List[String]]) = {
    val nums_tokens = data.map(_.size).toList
    val max_tokens = nums_tokens.max
    val min_tokens = nums_tokens.min
    val average_tokens = nums_tokens.sum / nums_tokens.size

    val nums_occur = nums_tokens.map(_ / 10).groupBy(identity).mapValues(_.size)
    logger.info(s"nums_of_code\tmax\tmin\taverage")
    logger.info(s"${nums_tokens.size}\t${max_tokens}\t${min_tokens}\t${average_tokens}")
    val names = nums_occur.map(e => s"[${e._1 * 10}, ${(e._1 + 1) * 10})").mkString("\t")
    val values = nums_occur.map(_._2).mkString("\t")
    logger.info("The total number of per each counting period, and step is 10")
    logger.info(s"\t${names}")
    logger.info(s"value\t${values}")
  }

  def build_vocab(buggyPath: List[String],
                  fixedPath: List[String],
                  isFile:Boolean = false) = {

    val buggyTokens = buggyPath.par.map { src => {
      val cu = getComplationUnit(src, METHOD, isFile)
      cu.getTokenRange.get().toList.filter(
        e => e.getText != " " && e.getText != "\n").map(_.getText)
     }
    }
    logger.info(s"[Buggy] Average")
    get_statistics(buggyTokens)

    val fixedTokens = fixedPath.par.map { src => {
      val cu = getComplationUnit(src, METHOD, isFile)
      cu.getTokenRange.get().toList.filter(
        e => e.getText != " " && e.getText != "\n").map(_.getText)
     }
    }
    logger.info(s"[Fixed]")
    get_statistics(fixedTokens)

    val allTokens = buggyTokens.flatMap(_.toSeq).union(fixedTokens.flatMap(_.toSeq)).filter(_ != "DummyClass")
    allTokens.groupBy(identity).mapValues(_.size).toList.sortBy(_._2)(Ordering[Int].reverse)
  }

  def run() = {
    val buggy_path = config.getString("buggy_path")
    val fixed_path = config.getString("fixed_path")
    val is_abstract = config.getBoolean("is_abstract")
    val top_k = config.getInt("top_k")

    val (buggy_files, fixed_files) = if (is_abstract)
      (readFile(buggy_path), readFile(fixed_path))
    else
      loadAndCheckData(buggy_path, fixed_path)

    val freqs = build_vocab(buggy_files, fixed_files, ! is_abstract)
    logger.info(s"The total vocab is ${freqs.size}")

    val idioms = freqs.filter(ele => !keywords.contains(ele._1)).map(_._1)

    if (top_k < idioms.size) idioms.take(top_k) else idioms
  }

}
