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

  def build_vocab(buggyPath: List[String],
                  fixedPath: List[String],
                  isFile:Boolean = false) = {

    val buggyTokens = buggyPath.par.map { src => {
      val cu = getComplationUnit(src, METHOD, isFile)
      cu.getTokenRange.get().toList.filter(
        e => e.getText != " " && e.getText != "\n").map(_.getText)
     }
    }
    val buggyStatis = buggyTokens.map(_.size)

    logger.info(s"[Buggy] Average: ${buggyStatis.sum / buggyStatis.size}, Min: ${buggyStatis.min}, Max: ${buggyStatis.max}")

    val fixedTokens = fixedPath.par.map { src => {
      val cu = getComplationUnit(src, METHOD, isFile)
      cu.getTokenRange.get().toList.filter(
        e => e.getText != " " && e.getText != "\n").map(_.getText)
     }
    }
    val fixedStatis = fixedTokens.map(_.size).toList

    logger.info(s"[Fixed] Average: ${fixedStatis.sum / fixedStatis.size}, Min: ${fixedStatis.min}, Max: ${fixedStatis.max}")

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
