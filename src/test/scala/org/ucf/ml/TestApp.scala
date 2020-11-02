package org.ucf.ml
/**
  * @author
  */
import org.junit.Test
import org.ucf.ml.parallel.ASTDiffMaster

class ScalaTestAPP extends TestUtils {
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

  @Test def testParallel(): Unit ={
    import scala.jdk.CollectionConverters._
    val config = Map[String, Object]("run_type" -> "abstract",
      "buggy_path"-> "data/small/raw/buggy/",
      "fixed_path" ->"data/small/raw/fixed/",
      "idioms_path" -> "data/idioms/idioms.csv",
      "with_position" -> false.asInstanceOf[Object],
      "output_position" -> false.asInstanceOf[Object],
      "output_dir" -> "data/small/processed/total/",
      "nums_worker" -> 10.asInstanceOf[Object]).asJava

    import net.sourceforge.argparse4j.inf.Namespace
    val worker = new parallel.AbstractMaster(new Namespace(config))
    worker.run()
  }


  @Test def testASTDiffParallel(): Unit ={
    import scala.jdk.CollectionConverters._
    val step = 100000
    val n_best = 1
    val nums_worker = 16
    val config = Map[String, Object]("run_type" -> "astdiff",
      "predt_path"-> s"data/small/predict/${step}/predictions_${n_best}_${n_best}.txt",
      "fixed_path" -> s"data/small/predict/${step}/test-fixed.txt",
      "buggy_path" -> s"data/small/predict/${step}/test-buggy.txt",
      "output_dir" -> "data/small/predict/",
      "n_best" -> n_best.asInstanceOf[Object],
      "nums_worker" -> nums_worker.asInstanceOf[Object]).asJava

    import net.sourceforge.argparse4j.inf.Namespace
    val worker = new ASTDiffMaster(new Namespace(config))
    worker.run()
  }


}