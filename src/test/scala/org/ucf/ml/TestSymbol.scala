package org.ucf.ml
import org.junit.Test
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import com.github.javaparser.symbolsolver.resolution.typesolvers.ReflectionTypeSolver
import java.io.File
import java.util.stream.Collectors

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.expr.{AssignExpr, MethodReferenceExpr}
import com.github.javaparser.symbolsolver.JavaSymbolSolver

class TestSymbol extends TestUtils {

  // https://stackoverflow.com/questions/40832961/identify-variables-used-in-a-statement-line

  @Test def testProjectResolver():Unit = {
    val srcPath = "src/main/java/org/ucf/ml/JavaApp.java"
    val src = new File(srcPath)
//    val typeSolver = new CombinedTypeSolver(new ReflectionTypeSolver())
    val cu = getComplationUnit(srcPath, CLASS)

//    cu.accept(TypeCalculatorVisitor(), JavaParserFacade.get(typeSolver))
    println(cu)
  }


  @Test def testTypeResolver(): Unit = {
    val srcPath = "src/main/java/org/ucf/ml/JavaApp.java"
    val src = new File(srcPath)

    val typeSolver = new CombinedTypeSolver()
    val symbolSolver = new JavaSymbolSolver(typeSolver)
    StaticJavaParser.getConfiguration.setSymbolResolver(symbolSolver)

    val cu = StaticJavaParser.parse(src)

    getAssignExpr(cu).foreach(ex => {
      println(ex.calculateResolvedType())
    })
  }

}
