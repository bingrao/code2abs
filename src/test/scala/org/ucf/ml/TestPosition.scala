package org.ucf.ml

import org.junit.Test

class TestPosition extends TestUtils{
  @Test def testPositionEmbedding() {
    val inputClass =
      """
        |public class JavaApp {
        |    public void hello(String input) {
        |        Int c = a + b;
        |    }
        |}
        |""".stripMargin

    val cu = getComplationUnit(inputClass, CLASS, false)

    printAST(outPath="log/test.Yaml", cu = cu, format = "ymal")
    printAST(outPath="log/test.dot", cu = cu, format = "dot")

    genPositionEmbedding(ctx, cu)
  }
}
