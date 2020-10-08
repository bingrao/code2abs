package org.ucf.ml
/**
 * @author
 */
import java.io.File
import java.nio.file.FileSystem

import gumtree.spoon.AstComparator
import gumtree.spoon.builder.SpoonGumTreeBuilder
import org.junit.Test

import scala.collection.mutable.ListBuffer
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



  def _test(input:File) = {
    var cnt = 1
    var fail_cnt = 0
    var succ_cnt = 0
    val source = Source.fromFile(input)
    for (line <- source.getLines()){
      try {
        val cu = getComplationUnit(line, METHOD, false)
        succ_cnt = succ_cnt + 1
        //        if (cu != None)
        //          logger.error(f"[${cnt}] Successfully")
      } catch {
        case e: Exception => {
          //          logger.info(f"[${cnt}] Failed: ${line}")
          fail_cnt = fail_cnt + 1
          //          logger.info(e.getMessage + "\n\n")
        }
      } finally {
        cnt = cnt + 1
      }
    }

//    logger.info(s"Total [${cnt}], succecced [${succ_cnt}], faled [${fail_cnt}]-${input}")
    source.close()
    List(cnt, succ_cnt, fail_cnt)
  }


  @Test def testGeneratedCode():Unit = {
    val buggy_path = "data/small/processed/buggy.txt"
    val fixed_path = "data/small/processed/fixed.txt"

    _test(new File(buggy_path))
    _test(new File(fixed_path))
  }

  @Test def testGeneratedCodeAll():Unit = {

    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

    val data_dir = "data/small/processed/reviews"
    val d = new File(data_dir)
    val datasets = recursiveListFiles(d).filter(file => file.isFile && !file.getName.contains("vocab"))
    val results = ListBuffer[String]()

    datasets.foreach(dataset => {
      val name = dataset.toString.split("\\\\").drop(4).mkString("\t")
      val count = _test(dataset).mkString("\t")
      println(name + "\t" + count)
    })
  }


}
