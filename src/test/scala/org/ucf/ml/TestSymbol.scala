package org.ucf.ml
import org.junit.Test

import com.github.javaparser.JavaParser
import com.github.javaparser.ParseException
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.MethodCallExpr
import com.github.javaparser.ast.stmt.ReturnStmt
import com.github.javaparser.ast.visitor.VoidVisitorAdapter
import com.github.javaparser.resolution.types.ResolvedReferenceType
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import com.github.javaparser.symbolsolver.model.resolution.TypeSolver
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import com.github.javaparser.symbolsolver.resolution.typesolvers.JavaParserTypeSolver
import com.github.javaparser.symbolsolver.resolution.typesolvers.ReflectionTypeSolver
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException


class TestSymbol extends TestUtils {



  @Test def testProjectResolver():Unit = {
    val srcPath = "src/main/java/org/ucf/ml/JavaApp.java"
    val src = new File(srcPath)
    val typeSolver = new CombinedTypeSolver(new ReflectionTypeSolver())
    val cu = getComplationUnit(srcPath, CLASS)

    cu.accept(TypeCalculatorVisitor(), JavaParserFacade.get(typeSolver))

  }

}
