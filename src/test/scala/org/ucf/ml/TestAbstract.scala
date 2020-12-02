package org.ucf.ml
/**
 * @author
 */
import org.junit.Test

class TestAbstract extends TestUtils {

  @Test def testAbstractAPI() {
    val inputClass =
      """
        | public void hello(String input) {
        |   Int c = a + b;
        | }
        |""".stripMargin
    val code = genAbstratCodeWithPosition(inputClass, granularity = METHOD)
    println(code)
  }

  @Test def testAbstract() {
    val inputClass =
      """
        |package org.ucf.java;
        |public class JavaApp {
        |    int bb = 4;
        |    int dd = 18;
        |    public void hello(String bb) {
        |        if (bb < 5) {
        |           int cc = 3;
        |        } else {
        |           dd = bb + 5;
        |        }
        |
        |        int a =3, b, i = ed;
        |        int ad = 7;
        |        Integer bb = ad + cc;
        |        for(i = 0; i < 10; i++){
        |           bb = bb + ed;
        |        }
        |        {
        |           int bb = 5;
        |           c  = bb + 3;
        |        }
        |    }
        |}
        |""".stripMargin
    get_abstract_code(inputClass, CLASS, isFile = false)
  }

  @Test def testAbstractFile_68(): Unit = {
    val input = "data/small/raw/fixed/1.java"
    get_abstract_code(input, METHOD)
  }

  @Test def testAbstractFile(): Unit ={
    val input = "data/small/raw/error/53486.java"
    get_abstract_code(input, METHOD)
  }

  @Test def testPairAbstract():Unit = {
    val file_index = 1
    val buggy = s"data/small/raw/buggy/$file_index.java"
    val fixed = s"data/small/raw/fixed/$file_index.java"
    get_abstract_code(buggy, METHOD, isFile = true, SOURCE)
    get_abstract_code(fixed, METHOD, isFile = true, TARGET)
  }


  @Test def testBytePairAbstract():Unit = {
    val file_index = 1
    val buggy = s"data/small/raw/buggy/$file_index.java"
    val fixed = s"data/small/raw/fixed/$file_index.java"
    logger.info(s"Buggy: ${getBytePairEncodingFromString(buggy)}")
    logger.info(s"Fixed: ${getBytePairEncodingFromString(fixed)}")
  }
}
