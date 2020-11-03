package org.ucf.ml

import gumtree.spoon.AstComparator
import scala.collection.mutable



case class SparkRunnerContext(buggy_batch: String = null,
                    fixed_batch: String = null,
                    predt_batch:List[String] = null,
                    work_id:Int = 0,
                    n_best:Int = 1) extends Serializable {

  def get_work_id:Int = this.work_id
  def get_n_best:Int = n_best

  private val buggy_abstract = new StringBuilder
  private val fixed_abstract = new StringBuilder
  private val predt_abstract = new StringBuilder


  def append_buggy(content:String): mutable.StringBuilder = this.buggy_abstract.append(content)
  def append_fixed(content:String): mutable.StringBuilder = this.fixed_abstract.append(content)
  def append_predt(content:String): mutable.StringBuilder = this.predt_abstract.append(content)

  def get_buggy_abstract:String = buggy_abstract.toString()
  def get_fixed_abstract:String = fixed_abstract.toString()
  def get_predt_abstract:String = predt_abstract.toString()

  def get_buggy = this.buggy_batch
  def get_fixed = this.fixed_batch
  def get_predt = this.predt_batch

  var cnt_0 = 0
  var cnt_1 = 0
  var cnt_2 = 0
  var cnt_3 = 0
  var cnt_4 = 0
  var cnt_error = 0
  var cnt_other = 0
}


object SparkRunner extends utils.Common {
  def main(args: Array[String]): Unit = {
    val buggy = args(0)
    val fixed = args(1)
    val predt = args(2)
    val n_best = args(3).toInt
    val nums_worker = args(4).toInt
    val output_dir = args(5)
    val log_file = args(6)

    logger.updateFileAppender(log_file)

    var cnt_0 = 0
    var cnt_1 = 0
    var cnt_2 = 0
    var cnt_3 = 0
    var cnt_4 = 0
    var cnt_error = 0
    var cnt_other = 0
    /* Load buggy and target files, and save their path as a list of string*/
    val (buggy_files, fixed_files, predt_files) = loadAndCheckData(buggy, fixed, predt, n_best)

    val total_files_nums = math.min(fixed_files.size, predt_files.size)
    logger.info(s"Loading buggy, fixed  and prdt files ${total_files_nums} with n_best ${n_best}")

    import org.apache.spark.sql.SparkSession
    val spark = SparkSession.builder().getOrCreate()

    val data = for (i <- 0 until total_files_nums)
      yield SparkRunnerContext(buggy_files(i), fixed_files(i), predt_files.slice(i * n_best, (i + 1) * n_best), i, n_best)

    val data_rdd = spark.sparkContext.parallelize(data).repartition(nums_worker)

    implicit class getASTDiffScore(ast: AstComparator){
      def getDiffScore(buggy:String, fixed:String) = {
        ast.compare(buggy, fixed)
      }
    }
    val results = data_rdd.map{ wtx => {
      val astdiff = new AstComparator()
      val src =  raw"public class DummyClass { ${wtx.get_fixed} }"
      val count = wtx.get_predt.map{ case predt => {
        val tgt = raw"public class DummyClass { ${predt} }"
        val result = try {
          val diff = astdiff.getDiffScore(src, tgt)
          Right(diff.getAllOperations.size())
        } catch {
          case e: Exception => Left(e)
        }
        result match {
          case Right(r) => r
          case Left(_) => Int.MaxValue
        }
      }}

      val min_count = count.min

      min_count match {
        case 0 => wtx.cnt_0 = wtx.cnt_0 + 1
        case 1 => wtx.cnt_1 = wtx.cnt_1 + 1
        case 2 => wtx.cnt_2 = wtx.cnt_2 + 1
        case 3 => wtx.cnt_3 = wtx.cnt_3 + 1
        case 4 => wtx.cnt_4 = wtx.cnt_4 + 1
        case Int.MaxValue => wtx.cnt_error = wtx.cnt_error + 1
        case _ => wtx.cnt_other = wtx.cnt_other + 1
      }
      val prefix = if (min_count == Int.MaxValue) s"Er" else s"${min_count}"
      wtx.append_predt(s"#${wtx.get_work_id}#${prefix}#\t" + wtx.get_predt(count.indexOf(min_count)))
      wtx.append_fixed(s"#${wtx.get_work_id}#${prefix}#\t" + wtx.get_fixed)
      wtx.append_buggy(s"#${wtx.get_work_id}#${prefix}#\t" + wtx.get_buggy)
      wtx
    }}.collect().sortBy(_.get_work_id)

    val predt_abstract = results.map(_.get_predt_abstract)
    val fixed_abstract = results.map(_.get_fixed_abstract)
    val buggy_abstract = results.map(_.get_buggy_abstract)

    write(output_dir + s"${n_best}_ast_predt_best.txt", predt_abstract.mkString("\n"))

    write(output_dir + s"${n_best}_ast_fixed_best.txt", fixed_abstract.mkString("\n"))

    write(output_dir + s"${n_best}_ast_buggy_best.txt", buggy_abstract.mkString("\n"))

    results.foreach(wtx => {
      cnt_0 = cnt_0 + wtx.cnt_0
      cnt_1 = cnt_1 + wtx.cnt_1
      cnt_2 = cnt_2 + wtx.cnt_2
      cnt_3 = cnt_3 + wtx.cnt_3
      cnt_4 = cnt_4 + wtx.cnt_4
      cnt_other = cnt_other + wtx.cnt_other  // > 4
      cnt_error = cnt_error + wtx.cnt_error // cannot pase
    })

    logger.info(s"[Performance]-[ast]-[${n_best}]-[${fixed_files.size}]\tcounting_[0-4, >4, error]: ${cnt_0} ${cnt_1} ${cnt_2} ${cnt_3} ${cnt_4} ${cnt_other} ${cnt_error}")

  }
}
