package org.ucf.ml
import org.junit.Test

import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import com.github.javaparser.symbolsolver.resolution.typesolvers.ReflectionTypeSolver
import java.io.File

class TestSymbol extends TestUtils {



  @Test def testProjectResolver():Unit = {
    val srcPath = "src/main/java/org/ucf/ml/JavaApp.java"
    val src = new File(srcPath)
    val typeSolver = new CombinedTypeSolver(new ReflectionTypeSolver())
    val cu = getComplationUnit(srcPath, CLASS)

    cu.accept(TypeCalculatorVisitor(), JavaParserFacade.get(typeSolver))

  }

}
