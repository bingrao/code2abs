package org.ucf.ml
package parser


import com.github.javaparser.{JavaToken, StaticJavaParser}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.printer.PrettyPrinterConfiguration

import scala.collection.JavaConversions._
import gumtree.spoon.AstComparator
import org.ucf.ml.utils.Context

import scala.collection.mutable.ListBuffer
import scala.io.Source


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
        case "xml" => new XmlPrinter(true).output(cu)
        case "dot" => new DotPrinter(true).output(cu)
        case "ymal" | _ => new YamlPrinter(true).output(cu)
      }
      if (outPath != null)
        write(outPath, context = context)
      else
        println(context)
    }
  } catch {
    case e:Exception =>{
      logger.error(f"Write file $outPath failed in the format of $format")}
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

//  def getBPEComplationUnit(sourcePath:String, ctx:Context,
//                           granularity:Value, isFile:Boolean = true): CompilationUnit = {
//    val cu = getComplationUnit(sourcePath, granularity, isFile)
//    if (ctx.bpe_enable) {
//      val bpe = getBytePairEncodingFromCompilation(cu)
//      bpe.foreach(ele => ctx.bpe_map.getNewContent(ele, 0))
//      var source = Granularity.apply(sourcePath, granularity, isFile).getSourceCode()
//
//      ctx.bpe_map.getData.foreach {
//        case (key, value) => {
//          val replace_source = source
//          source = replace_source.replace(key, value(0).split("_")(0))
//        }
//      }
//      println(source)
//      StaticJavaParser.parse(source)
//    } else cu
//  }

  def getBPEComplationUnit(sourcePath:String, ctx:Context,
                           granularity:Value, isFile:Boolean = true): CompilationUnit = {
    val cu = getComplationUnit(sourcePath, granularity, isFile)
    if (ctx.bpe_enable) {
      val bpe = getBytePairEncodingFromCompilation(cu)
      bpe.foreach(ele => ctx.bpe_map.getNewContent(ele, 0))
    }
    cu
  }



  implicit class getASTDiffScore(ast: AstComparator){
    def getDiffScore(buggy:String, fixed:String) = {
      ast.compare(buggy, fixed)
    }
  }

  def getASTDiff(srcPath:String, tgtPath:String, granularity:Value=METHOD, isFile:Boolean = true) = {
    val src = Granularity.apply(srcPath, granularity, isFile).getSourceCode()
    val tgt = Granularity.apply(tgtPath, granularity, isFile).getSourceCode()
    val compare = new AstComparator()
    val diff = compare.getDiffScore(src, tgt)
    diff
  }

  def getASTDiffCount(srcPath:String, tgtPath:String, granularity:Value=METHOD, isFile:Boolean = true) = {
    val diff = getASTDiff(srcPath, tgtPath, granularity, isFile)
    diff.getAllOperations.size()
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


  def genCombinedFiles(buggy_dir:String,
                       fixed_dir:String,
                       output_dir:String): Unit = {

    val buggy_src = ListBuffer[String]()
    val fixed_src = ListBuffer[String]()

    val config = new PrettyPrinterConfiguration()
    config.setEndOfLineCharacter("")
    config.setIndentSize(1)
    config.setTabWidth(1)
    var nums_line = 0
    var cnt = 0
    var fail_cnt = 0
    val (buggy_files, fixed_files) = loadAndCheckData(buggy_dir, fixed_dir)

    for ((buggy, fixed) <- buggy_files.zip(fixed_files)) {

      try {
        val buggy_cu = getComplationUnit(buggy, METHOD, true)
        val fixed_cu = getComplationUnit(fixed, METHOD, true)
        val buggy_class = getClassOrInterfaceDeclaration(buggy_cu).filter(c => c.getNameAsString.equals("DummyClass"))
        val fixed_class = getClassOrInterfaceDeclaration(fixed_cu).filter(c => c.getNameAsString.equals("DummyClass"))

        val buggy_m = if (buggy_class.nonEmpty) {
          val dummpy = buggy_class(0)
          val members = dummpy.getMembers
          if (members.nonEmpty) members(0) else {
            logger.info(s"[Failed]-Method: $buggy")
            null
          }
        } else {
          logger.info(s"[Failed]-Class: $buggy")
          null
        }
        val fixed_m = if (fixed_class.nonEmpty) {
          val dummpy = fixed_class(0)
          val members = dummpy.getMembers
          if (members.nonEmpty) members(0) else {
            logger.info(s"[Failed]-Method: $fixed")
            null
          }
        } else {
          logger.info(s"[Failed]-Class: $fixed")
          null
        }

        if (buggy_m != null && fixed_m != null) {
          buggy_src.append(buggy_m.toString(config))
          fixed_src.append(fixed_m.toString(config))
          cnt = cnt + 1
        }
      } catch {
        case e: Exception => {
          fail_cnt = fail_cnt + 1
        }
      } finally {
        nums_line = nums_line + 1
      }
    }
    logger.info(s"The total $nums_line, found $cnt, failed $fail_cnt")
    write(s"$output_dir/buggy_src.txt", buggy_src.mkString("\n"))
    write(s"$output_dir/fixed_src.txt", fixed_src.mkString("\n"))
  }


  def genSequencerData(src_path:String,
                       tgt_path:String,
                       output_dir:String,
                       idioms_path:String,
                       max_length:Int = 2400): Unit = {
    val src_source = Source.fromFile(src_path).getLines()
    val tgt_source = Source.fromFile(tgt_path).getLines()
    var cnt = 1
    var fail_cnt = 0
    var succ_cnt = 0
    var line_nums = 0
    var find_nums = 0
    val idioms = readIdioms(idioms_path)

    val buggy = ListBuffer[String]()
    val fixed = ListBuffer[String]()

    val buggy_src = ListBuffer[String]()
    val fixed_src = ListBuffer[String]()

    val config = new PrettyPrinterConfiguration()
    config.setEndOfLineCharacter("")
    config.setIndentSize(1)
    config.setTabWidth(1)

    def _task(ctx:Context, cu:CompilationUnit, mode:Value) = {
      ctx.setCurrentMode(mode)
      genAbstractCode(ctx, cu)
    }

    for ((src, tgt) <- src_source.zip(tgt_source)) {
      line_nums = line_nums + 1
//      if (src.contains("<START_BUG>") && src.contains("<END_BUG>") && src.size < max_length) {
      if (src.contains("<START_BUG>") && src.contains("<END_BUG>")) {
        try {
          val new_source = src.replace("<START_BUG>", "int START_BUG = 0;")
            .replace("<END_BUG>", "int END_BUG = 0;")

          val cu = getComplationUnit(new_source, CLASS, false)

          val method = getMethodDecl(cu).filter(m => {
            val names = ListBuffer[String]()
            VariableDecl().visit(m, names)
            names.contains("START_BUG")
          })

          method.foreach(m => {
            val src = m.toString(config).replace("int START_BUG = 0;", "<START_BUG>")
              .replace("int END_BUG = 0;", "<END_BUG>")

            val src_list = src.split(" ")

            val start_index = src_list.indexOf("<START_BUG>")
            val end_index = src_list.indexOf("<END_BUG>")

            val src_first = src_list.take(start_index)
            val src_last = src_list.takeRight(src_list.size - end_index - 1)

            val tgt_list = tgt.split(" ")

            val new_source = src.replace("<START_BUG>", "").replace("<END_BUG>", "")
            val new_target = src_first.++(tgt_list).++(src_last).mkString(" ")

            val src_cu = getComplationUnit(new_source, METHOD, false)
            val tgt_cu = getComplationUnit(new_target, METHOD, false)


            val context = new Context(idioms = idioms, METHOD)
            _task(context, src_cu, SOURCE)
            _task(context, tgt_cu, TARGET)

            buggy.append(context.get_buggy_abstract())
            buggy_src.append(new_source)
            fixed.append(context.get_fixed_abstract())
            fixed_src.append(new_target)

            find_nums = find_nums + 1

          })
          succ_cnt = succ_cnt + 1
        } catch {
          case e: Exception => {
            fail_cnt = fail_cnt + 1
          }
        } finally {
          cnt = cnt + 1
        }
      }
    }
    logger.info(s"Total [${line_nums}-${cnt}], succecced [${succ_cnt}], faled [${fail_cnt}], find [${find_nums}]")

    logger.info(s"Buggy code: ${buggy.size}, Fixed code: ${fixed.size}")

    write(s"${output_dir}/buggy_abstract.txt", buggy.mkString("\n"))
    write(s"${output_dir}/buggy_src.txt", buggy_src.mkString("\n"))

    write(s"${output_dir}/fixed_abstract.txt", fixed.mkString("\n"))
    write(s"${output_dir}/fixed_src.txt", fixed_src.mkString("\n"))
  }


  /******************************************  [[APIs for Python Call]] **********************************************/
  /**
   *  The following four APIs is provided to work with python script to get abstract code
   *  Step 1: Execute Scala code [[GatewayServer]] to initial a py4j gateway server with [[JavaParser]] entry point
   *  >>> java -Dlog4j.configuration=file:///home/bing/project/OpenNMT-py/examples/learning_fix/config/log4j.properties
   *           -cp examples/learning_fix/bin/java_abstract-1.0-jar-with-dependencies.jar org.ucf.ml.GatewayServer 25333
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
