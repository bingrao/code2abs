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
    val code = genAbstratCodeWithPosition(inputClass, granularity = METHOD, isFile = false)
    println(code)
  }

  @Test def testAbstract() {
    val inputClass =
      """
        |package org.ucf.java;
        |public class JavaApp {
        |    int bb = 3;
        |    public void hello(String input) {
        |        if (bb < 5) {
        |           int cc = 3;
        |        } else {
        |           dd = bb + 5;
        |        }
        |
        |        int a =3, b, i = ed;
        |        int ad = 7;
        |        Integer bb = ad + cc;
        |        for(i = 0; i < 10; i++)
        |           bb = bb + ed;
        |    }
        |}
        |""".stripMargin
    get_abstract_code(inputClass, CLASS, false)
  }

  @Test def testAbstractFile_68(): Unit = {
    val input = "data/small/raw/fixed/1.java"
    get_abstract_code(input, METHOD, true)
  }

  @Test def testAbstractFile(): Unit ={
    val input = "data/small/raw/buggy/1.java"
    get_abstract_code(input, METHOD, true)
  }

  @Test def testPairAbstract():Unit = {
    val file_index = 1
    val buggy = s"data/small/raw/buggy/${file_index}.java"
    val fixed = s"data/small/raw/fixed/${file_index}.java"
    single_task(buggy, fixed)
  }

}
