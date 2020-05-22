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

  /**
   *
   * @param sourcePath
   * @param granularity
   * @param isFile
   * @return
   */
  def getComplationUnit(sourcePath:String, granularity:Value, isFile:Boolean = true):CompilationUnit = {
    val source = Granularity.apply(sourcePath, granularity, isFile).getSourceCode()
    StaticJavaParser.parse(source)
  }

  def genAbstratCodeWithPosition(sourcePath:String, idiomPath:String = "data/idioms/idioms.csv",
                                 granularity:Value = METHOD, isFile:Boolean = false) = {
    val project_idioms = readIdioms(idiomPath)
    val context = new Context(project_idioms, granularity)
    val cu = getComplationUnit(sourcePath, granularity, isFile)

    /**Traverse AST to generate corresponding node's positional embedding**/
    genPositionEmbedding(context, cu)

    /**Traverse AST to gen abstract code**/
    genAbstractCode(context, cu)

    context.get_buggy_abstract()
  }

  /******************************************  [[APIs for Python Call]] **********************************************/
  /**
   *  The following four APIs is provided to work with python script to get abstract code
   *  Step 1: Execute Scala code [[GatewayServer]] to initial a py4j gateway server with [[JavaParser]] entry point
   *
   *  Step 2: Execute following python to load [[JavaGateWay]] interface to communicate with jvm to retrieve
   *  corresponding objects. Please be aware that if you use relative path for your input parameters, you need them
   *  to be relative to [[GatewayServer]] path, rather than the currenting python executing path.
   *
   *  >>> from py4j.java_gateway import JavaGateway
   *  >>> gateway = JavaGateway()
   *  >>> input = "void method(String input) { Int a = b + 1; }"
   *  >>> gateway.entry_point.getAbstractCodeFromStringMethod(input, "idioms/idioms.csv")
   *  'void method ( String Varl_0 ) { Type_0 a = b + 1 ; } '
   *
   *  Reference: https://www.py4j.org/getting_started.html
   */
  def getAbstractCodeFromStringMethod(sourcePath:String, idiomPath:String = "idioms/idioms.csv") =
    genAbstratCodeWithPosition(sourcePath, idiomPath, METHOD, false)

  def getAbstractCodeFromStringClass(sourcePath:String, idiomPath:String = "idioms/idioms.csv") =
    genAbstratCodeWithPosition(sourcePath, idiomPath, CLASS, false)

  def getAbstractCodeFromFileMethod(sourcePath:String, idiomPath:String = "idioms/idioms.csv") =
    genAbstratCodeWithPosition(sourcePath, idiomPath, METHOD, true)

  def getAbstractCodeFromFileSClass(sourcePath:String, idiomPath:String = "idioms/idioms.csv") =
    genAbstratCodeWithPosition(sourcePath, idiomPath, CLASS, true)


  @deprecated
  def getTokens(cu:CompilationUnit) = cu.getTokenRange.get().toList

  @deprecated
  def readTokens(filePath:String, granularity:Value, isFile:Boolean = true):List[JavaToken] = {
    val cu = this.getComplationUnit(filePath, granularity)
    this.getTokens(cu)
  }
}
