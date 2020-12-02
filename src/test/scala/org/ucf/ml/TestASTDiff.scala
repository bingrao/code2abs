package org.ucf.ml
/**
 * @author
 */
import java.io.File

import gumtree.spoon.AstComparator
import org.junit.Test

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.JavaConversions._


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

  @Test def testAstDiff(): Unit ={
    val file_index = 0
    val buggy = s"data/small/raw/buggy/${file_index}.java"
    val fixed = s"data/small/raw/fixed/${file_index}.java"
    val editScript  = getASTDiff(buggy, fixed, METHOD)
    val all_actitions = editScript.getAllOperations()
    println(all_actitions)

    val root_acitions = editScript.getRootOperations()

    println("\n" + root_acitions)

    println(all_actitions.size(), root_acitions.size())
  }

  def perf_count(n_best:Int = 1): Unit = {
    import scala.io.Source
    val step = 79000
    val fixed_pred = s"data/small/predict/${step}/predictions_${n_best}_${n_best}.txt"
    val fixed_pred_best = s"data/small/predict/${step}/predictions_${n_best}_${n_best}_best.txt"
    val fixed_src = s"data/small/predict/${step}/test-fixed.txt"
    var cnt_0 = 0
    var cnt_1 = 0
    var cnt_2 = 0
    var cnt_3 = 0
    var cnt_others = 0
    var cnt = 0
    var fail_cnt = 0

    for ((src, pred) <- Source.fromFile(fixed_src).getLines zip Source.fromFile(fixed_pred_best).getLines ) {
      try {
        val editScript = getASTDiff(src, pred, METHOD, false)
        editScript.getAllOperations.size() match {
          case 0 => cnt_0 = cnt_0 + 1
          case 1 => cnt_1 = cnt_1 + 1
          case 2 => cnt_2 = cnt_2 + 1
          case 3 => cnt_3 = cnt_3 + 1
          case _ => cnt_others = cnt_others + 1
        }
      } catch {
        case e: Exception => {
          fail_cnt = fail_cnt + 1
        }
      } finally {
        cnt = cnt + 1
      }

      if (cnt % 2000 == 0)
        logger.info(s"[${n_best}]-Total: ${cnt}, cnt_0: ${cnt_0}, cnt_1: ${cnt_1}, cnt_2: ${cnt_2}, cnt_3: ${cnt_3}, others: ${cnt_others}")
    }
    logger.info(s"[${n_best}]-Total: ${cnt}, cnt_0: ${cnt_0}, cnt_1: ${cnt_1}, cnt_2: ${cnt_2}, cnt_3: ${cnt_3}, others: ${cnt_others}")
  }


  @Test def testPerformance(): Unit = {
    for(n_best <- List(1, 5, 10, 15)){
      perf_count(n_best)
    }
  }

  @Test def testBugPerf():Unit = {



    import scala.io.Source
    val step = 79000
    val n_best:Int = 1
    val buggy_src = readFile(s"data/small/predict/${step}/test-buggy.txt")
    val fixed_src =  readFile(s"data/small/predict/${step}/test-fixed.txt")
    val fixed_pred_best =  readFile(s"data/small/predict/${step}/predictions_${n_best}_${n_best}_best.txt")
    val actions:ListBuffer[(String, String)] = new ListBuffer[(String, String)]()
    val astdiff = new AstComparator()

    for (i <- buggy_src.indices) {
      val context = SparkRunnerContext(buggy_src(i),
        fixed_src(i),
        fixed_pred_best.slice(i * n_best, (i + 1) * n_best),
        i,
        n_best)
      val results = context.run(new AstComparator())
    }
  }

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

    logger.info(s"Total [${cnt}], succecced [${succ_cnt}], faled [${fail_cnt}]-${input}")
    source.close()
    List(cnt, succ_cnt, fail_cnt)
  }


  @Test def testGeneratedCode():Unit = {
    val buggy_path = "data/small/processed/buggy.txt"
    val fixed_path = "data/small/processed/fixed.txt"

    val buggy_files = readFile(buggy_path)
    val fixed_files = readFile(fixed_path)

    for(i <- buggy_files.indices) {
      if (buggy_files(i) == fixed_files(i)) {
        logger.info(s"[${i+1}]: ${buggy_files(i)}")
      }
    }

    _test(new File(buggy_path))
    _test(new File(fixed_path))
//    val n_best = 15
//    _test(new File(s"data/small/predict/79000/predictions_${n_best}_${n_best}_best.txt"))
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
