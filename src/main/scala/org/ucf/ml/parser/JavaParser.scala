package org.ucf.ml
package parser


import com.github.javaparser.{JavaToken, StaticJavaParser}
import com.github.javaparser.ast.CompilationUnit
import scala.collection.JavaConversions._

class JavaParser extends Visitor  {

  /**
   * https://javaparser.org/inspecting-an-ast/
   * printAST("./log/UnionMain.Yaml", cu)
   * printAST("./log/UnionMain.Xml", cu, "xml")
   * printAST("./log/UnionMain.dot", cu, "dot")
   *
   * @param outPath
   * @param cu
   * @param format
   */

  def printAST(outPath:String=null, cu: CompilationUnit, format:String = null) = try {
    if ((outPath == null) && (format == null)){
      logger.info("The input source code is: ")
      println(cu.toString)
    } else {
      import com.github.javaparser.printer.{DotPrinter, XmlPrinter, YamlPrinter}
      val context = format match {
        case "xml" => (new XmlPrinter(true)).output(cu)
        case "dot" => (new DotPrinter(true)).output(cu)
        case "ymal" | _ => (new YamlPrinter(true)).output(cu)
      }
      if (outPath != null)
        write(outPath, context = context)
      else
        println(context)
    }
  } catch {
    case e:Exception =>{
      logger.error(f"Write file ${outPath} failed in the format of ${format}")}
      e.printStackTrace()
  }

  def getComplationUnit(sourcePath:String, granularity:Value, isFile:Boolean = true) = {
    val source = Granularity.apply(sourcePath, granularity, isFile).getSourceCode()
    StaticJavaParser.parse(source)
  }

  def getTokens(cu:CompilationUnit) = cu.getTokenRange.get().toList

  def readTokens(filePath:String, granularity:Value, isFile:Boolean = true):List[JavaToken] = {
    val cu = this.getComplationUnit(filePath, granularity)
    this.getTokens(cu)
  }
}
