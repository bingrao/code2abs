package org.ucf.ml
/**
 * @author
 */
import java.io.File

import gumtree.spoon.AstComparator
import gumtree.spoon.builder.SpoonGumTreeBuilder
import org.junit.Test
import scala.io.Source

class TestASTDiff extends TestUtils {
  @Test def testAdd() {
    val inputClass =
      """
        |package org.ucf.ml;
        |
        |import java.util.Arrays;
        |import java.util.List;
        |import org.apache.commons.lang3.StringUtils;
        |
        |public class JavaApp {
        |
        |    public void hello(String input) {
        |        List<String> messages = Arrays.asList("hello", "baeldung", "readers!");
        |        messages.forEach(word -> StringUtils.capitalize(word));
        |        messages.forEach(StringUtils::capitalize);
        |
        |        List<Integer> numbers = Arrays.asList(5, 3, 50, 24, 40, 2, 9, 18);
        |        numbers.stream().sorted((a, b) -> a.compareTo(b));
        |        numbers.stream().sorted(Integer::compareTo);
        |    }
        |}
        |""".stripMargin

    val cu = getComplationUnit(inputClass, CLASS, false)
    printAST(outPath=null, cu = cu, format = "ymal")
  }
//  @Test def testAstDiff(): Unit ={
//    val file_index = 1
//    val buggy = s"data/small/raw/buggy/${file_index}.java"
//    val fixed = s"data/small/raw/fixed/${file_index}.java"
//    val editScript  = getASTDiff(buggy, fixed, METHOD)
//    val all_actitions = editScript.getAllOperations()
//    println(all_actitions)
//
//
//
//    val root_acitions = editScript.getRootOperations()
//
//    println("\n" + root_acitions)
//
//    println(all_actitions.size(), root_acitions.size())
//  }
//




  @Test def testGeneratedCode():Unit = {
    val input = "data/small/processed/fixed.txt"
    var cnt = 1
    val source = Source.fromFile(input)
    for (line <- source.getLines()){
      try {
        val cu = getComplationUnit(line, METHOD, false)
//        if (cu != None)
//          logger.error(f"[${cnt}] Successfully")
      } catch {
        case e: Exception => {
          logger.info(f"[${cnt}] Failed: ${line}")
//          logger.info(e.getMessage + "\n\n")
        }
      } finally {
        cnt = cnt + 1
      }
    }

    logger.info(s"[${cnt}] - Susscessfully - The input file ${input}")
    source.close()
  }

}
