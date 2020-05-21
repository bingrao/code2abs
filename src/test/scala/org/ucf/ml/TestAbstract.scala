package org.ucf.ml
/**
 * @author
 */
import org.junit.Test
import org.junit.Assert._

class TestAbstract extends TestUtils {

  @Test def testAbstractAPI() {
    val inputClass =
      """
        | public void hello(String input) {
        |   Int c = a + b;
        | }
        |""".stripMargin
    val code = getAbstractCode(inputClass, granularity = METHOD, isFile = false)
    println(code)
  }

  @Test def testAbstract() {
    val inputClass =
      """
        |public class JavaApp {
        |    public void hello(String input) {
        |        Int c = a + b;
        |    }
        |}
        |""".stripMargin
    get_abstract_code(inputClass, CLASS, false)
  }

  @Test def testAbstractFile_68(): Unit ={
    val input = "data/raw/fixed/1.java"
    get_abstract_code(input, METHOD, true)
  }

  @Test def testAbstractFile(): Unit ={
    val input = "data/raw/buggy/1.java"
    get_abstract_code(input, METHOD, true)
  }

  @Test def testPairAbstract():Unit = {
    val file_index = 1
    val buggy = s"data/raw/buggy/${file_index}.java"
    val fixed = s"data/raw/fixed/${file_index}.java"
    single_task(buggy, fixed)
  }

}
