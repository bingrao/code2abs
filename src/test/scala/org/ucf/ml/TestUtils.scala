package org.ucf.ml

import java.io.File

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.{NameExpr, SimpleName}

trait TestUtils extends parser.JavaParser with utils.Common {
  // Load data idioms
  private val idioms = readIdioms()
  val ctx = new Context(idioms)

  def get_abstract_code(sourcePath:String, granularity:Value, isFile:Boolean = true) = {

    ctx.setCurrentMode(SOURCE)
    ctx.setGranularity(granularity)

    val cu = getComplationUnit(sourcePath, granularity, isFile)

    printAST(outPath="log/test.Yaml", cu = cu, format = "ymal")
    printAST(outPath="log/test.dot", cu = cu, format = "dot")

    /**Traverse AST to generate corresponding node's positional embedding**/
    genPositionEmbedding(ctx, cu)

    /**Traverse AST to gen abstract code**/
    genAbstractCode(ctx, cu)

    logger.info(s"Position nums ${ctx.positionalEmbedding.size}")

    ctx.positionalEmbedding.foreach{case (key, value) => {

      val name = if (key.isInstanceOf[SimpleName])
        key.asInstanceOf[SimpleName].asString()
      else if (key.isInstanceOf[NameExpr])
        key.asInstanceOf[NameExpr].getNameAsString
      else EmptyString
      println("%-40s %s".format(key.getClass.getName.split("\\.").last + s"[${name}]", value.toString()))
    }}

    println(cu)
    println("***************************************************")
    println(ctx.get_buggy_abstract)
    println("***************************************************")
    ctx.dumpy_mapping()
    println("***************************************************")
    ctx.buggy_toString
    println("***************************************************")
    println(ctx.get_buggy_abstract)
  }



  def single_abstract_task(inputPath:String, mode:Value, granularity:Value = METHOD) = {

    ctx.setCurrentMode(mode)

    if (logger.isDebugEnabled) ctx.append(s"[${mode}-${new File(inputPath).getName}]\t")

    val cu = getComplationUnit(inputPath, granularity)

    /**Traverse AST to generate corresponding node's positional embedding**/
    genPositionEmbedding(ctx, cu)

    /**Traverse AST to gen abstract code**/
    genAbstractCode(ctx, cu)

    if (logger.isDebugEnabled) {
      logger.debug(f"process ${mode} Source code ${inputPath}")
      printAST(outPath=f"log/test-${mode}.Yaml", cu = cu, format = "ymal")
      println(cu)
      println(ctx.get_abstract_code)
      println("******************************************************\n")
    }
  }

  def single_task(buggyPath:String, fixedPath:String, last:Boolean=false) = {

    if ((logger.isDebugEnabled) && (new File(buggyPath).getName != new File(fixedPath).getName)) {
      logger.error(s"[Input]-${buggyPath} != ${fixedPath}")
    }
    single_abstract_task(buggyPath, SOURCE)
    single_abstract_task(fixedPath, TARGET)
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

