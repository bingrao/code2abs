package org.ucf.ml

import java.io.File

import com.github.javaparser.ast.{CompilationUnit, Node}
import com.github.javaparser.ast.expr.{NameExpr, SimpleName}
import org.ucf.ml.utils.Context

import scala.collection.mutable.ListBuffer

trait TestUtils extends parser.JavaParser with utils.Common {
  // Load data idioms
  val idioms = readIdioms()
  val ctx = new Context(idioms)

  def get_abstract_code(sourcePath:String, granularity:Value = METHOD, isFile:Boolean = true, mode:Value = SOURCE): Unit = {

    ctx.setCurrentMode(mode)
    ctx.setGranularity(granularity)

    val cu = getComplationUnit(sourcePath, granularity, isFile)

    val scopes = new ListBuffer[Node]()

    ScopeCollector().visit(cu, scopes)

    scopes.foreach(println _)


    printAST(outPath=s"logs/${mode}-test.Yaml", cu = cu, format = "ymal")
    printAST(outPath=s"logs/${mode}-test.dot", cu = cu, format = "dot")

    /**Traverse AST to generate corresponding node's positional embedding**/
    genPositionEmbedding(ctx, cu)

    /**Traverse AST to gen abstract code**/
    genAbstractCode(ctx, cu)

//    logger.info(s"Position nums ${ctx.positionalEmbedding.size}")
//
//    ctx.positionalEmbedding.foreach{case (key, value) =>
//
//      val name = if (key.isInstanceOf[SimpleName])
//        key.asInstanceOf[SimpleName].asString()
//      else if (key.isInstanceOf[NameExpr])
//        key.asInstanceOf[NameExpr].getNameAsString
//      else EmptyString
//      println("%-40s %s".format(key.getClass.getName.split("\\.").last + s"[${name}]", value.toString()))
//    }

    println("***************************************************")
    println(cu)

    println("***************************************************")
    println(ctx.get_abstract_code)
    val abstract_ast = getComplationUnit(ctx.get_abstract_code, granularity, false)

    println("***************************************************")
    println(abstract_ast)


    println("***************************************************")
    ctx.dumpy_mapping()

  }



  def single_abstract_task(inputPath:String, mode:Value, granularity:Value = METHOD, isFile:Boolean=true) = {

    ctx.setCurrentMode(mode)

    if (logger.isDebugEnabled) ctx.append(s"[$mode-${new File(inputPath).getName}]\t")

    val cu = getComplationUnit(inputPath, granularity, isFile)

//    /**Traverse AST to generate corresponding node's positional embedding**/
//    genPositionEmbedding(ctx, cu)

    /**Traverse AST to gen abstract code**/
    genAbstractCode(ctx, cu)

    if (logger.isDebugEnabled) {
      logger.debug(f"process ${mode} Source code ${inputPath}")
      printAST(outPath=f"logs/test-${mode}.Yaml", cu = cu, format = "ymal")
      println(cu)
      println(ctx.get_abstract_code)
      println("******************************************************\n")
    }
  }

  def single_task(buggyPath:String, fixedPath:String, isFile:Boolean=false) = {

    if ((logger.isDebugEnabled) && (new File(buggyPath).getName != new File(fixedPath).getName)) {
      logger.error(s"[Input]-${buggyPath} != ${fixedPath}")
    }
    get_abstract_code(buggyPath, METHOD, isFile, SOURCE)
    get_abstract_code(fixedPath, METHOD, isFile, TARGET)
    val buggy = ctx.get_buggy_abstract()
    val fixed = ctx.get_fixed_abstract()
    (buggy, fixed)
  }


  def genAndPrintPositionEmbedding(ctx:Context, cu:CompilationUnit) = {
    genPositionEmbedding(ctx, cu)
    logger.info(s"Position nums ${ctx.positionalEmbedding.size}")

    ctx.positionalEmbedding.foreach{case (key, value) => {

      val name = if (key.isInstanceOf[SimpleName])
        key.asInstanceOf[SimpleName].asString()
      else if (key.isInstanceOf[NameExpr])
        key.asInstanceOf[NameExpr].getNameAsString
      else EmptyString

      println(s"${value.reverse.toString()} <- ${key.getClass.getName}-[${name}]")
    }}
  }

}

