package org.ucf.ml

import gumtree.spoon.AstComparator

import scala.collection.mutable
import com.github.javaparser.StaticJavaParser

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

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

  def get_buggy:String = this.buggy_batch
  def get_fixed:String = this.fixed_batch
  def get_predt:List[String] = this.predt_batch

  var cnt_0 = 0
  var cnt_1 = 0
  var cnt_2 = 0
  var cnt_3 = 0
  var cnt_4 = 0
  var cnt_error = 0
  var cnt_other = 0

  var cnt_syntax = 0.0
  var cnt_op_hit = 0.0
  var cnt_op_miss = 0.0
  var cnt_op_bad = 0.0

  val actions:ListBuffer[(String, String)] = new ListBuffer[(String, String)]()


  def isParsed(code:String):Boolean = {
    val is_parsed = try {
      StaticJavaParser.parse(code)
      Right(true)
    } catch {
      case e: Exception => Left(false)
    }
    is_parsed.isRight
  }

  def run(astdiff: AstComparator) = {
    val buggy =  raw"public class DummyClass { ${this.get_buggy} }"
    val fixed =  raw"public class DummyClass { ${this.get_fixed} }"

    val is_parsed = this.get_predt.count(code => this.isParsed(raw"public class DummyClass { $code }"))
    this.cnt_syntax = is_parsed * 1.0 / n_best

//    val buggy_fixed_diff = astdiff.compare(buggy, fixed).getAllOperations.toList

    val buggy_fixed_diff = try {
      Right(astdiff.compare(buggy, fixed).getAllOperations.toList)
    } catch {
      case e: Exception => Left(e)
    }


    val statis = this.get_predt.map { p => {
      val predt = raw"public class DummyClass { $p }"
      val result = try {
        val fixed_predt_diff = astdiff.compare(fixed, predt).getAllOperations.toList
        val buggy_predt_diff = astdiff.compare(buggy, predt).getAllOperations.toList

        val buggy_fixed_diff_docs = if (buggy_fixed_diff.isRight)
          buggy_fixed_diff.right.get.map(_.toString.replace("\n", " ")
          .replace("\t", " ").replace("\r", " "))
        else
          List(EmptyString)

        val buggy_predt_diff_docs = buggy_predt_diff.map(_.toString.replace("\n", " ")
          .replace("\t", " ").replace("\r", " "))

        val hit_count = buggy_predt_diff_docs.count(docs => buggy_fixed_diff_docs.contains(docs))
        val miss_count = buggy_fixed_diff_docs.length - hit_count
        val bad_count =  buggy_predt_diff_docs.length - hit_count

        val hit_ratio = hit_count * 1.0 / (if (buggy_fixed_diff_docs.isEmpty) 1 else buggy_fixed_diff_docs.size)
        val miss_ratio = miss_count * 1.0 / (if (buggy_fixed_diff_docs.isEmpty) 1 else buggy_fixed_diff_docs.size)
        val bad_ratio = bad_count * 1.0 / (if (buggy_fixed_diff_docs.isEmpty) 1 else buggy_fixed_diff_docs.size)

        val size = fixed_predt_diff.size

        val actions = if ((size == 0) && (buggy_fixed_diff.isRight)) {
          buggy_fixed_diff.right.get.map(op => {
            op.getAction.getClass.getSimpleName ->
              op.toString.replace("\r", " ").replace("\n", " ")
                .replace("\t", " ")
          })
        } else List[(String, String)]()

        Right((hit_ratio, miss_ratio, bad_ratio, size, actions))
      } catch {
        case e: Exception => Left(e)
      }
      if (result.isRight) {
        result.right.get
      } else
        (0.0, 0.0, 0.0, Int.MaxValue, List[(String, String)]())
    }}

    this.cnt_op_hit = statis.map(_._1).sum / n_best
    this.cnt_op_miss = statis.map(_._2).sum / n_best
    this.cnt_op_bad = statis.map(_._3).sum / n_best

    val count = statis.map(_._4)
    val min_count = count.min

    min_count match {
      case 0 => this.cnt_0 = this.cnt_0 + 1
      case 1 => this.cnt_1 = this.cnt_1 + 1
      case 2 => this.cnt_2 = this.cnt_2 + 1
      case 3 => this.cnt_3 = this.cnt_3 + 1
      case 4 => this.cnt_4 = this.cnt_4 + 1
      case Int.MaxValue => this.cnt_error = this.cnt_error + 1
      case _ => this.cnt_other = this.cnt_other + 1
    }
    val prefix = if (min_count == Int.MaxValue) s"Er" else s"${min_count}"
    this.append_predt(s"#${this.get_work_id}#${prefix}#\t" + this.get_predt(count.indexOf(min_count)))
    this.append_fixed(s"#${this.get_work_id}#${prefix}#\t" + this.get_fixed)
    this.append_buggy(s"#${this.get_work_id}#${prefix}#\t" + this.get_buggy)

    val perf_actions = statis.map(_._5).filter(_.nonEmpty)
    if (perf_actions.nonEmpty)
      this.actions.++=(perf_actions.head)

    this
  }
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

    val results = data_rdd.map(_.run(new AstComparator())).collect().sortBy(_.get_work_id)

    val predt_abstract = results.map(_.get_predt_abstract)
    val fixed_abstract = results.map(_.get_fixed_abstract)
    val buggy_abstract = results.map(_.get_buggy_abstract)

    val predt_actions = results.filter(_.actions.nonEmpty).flatMap(wtx => {
      wtx.actions.map(op => s"${wtx.get_work_id} ${op._2}").toList
    })

    write(output_dir + s"${n_best}_ast_predt_best.txt", predt_abstract.mkString("\n"))

    write(output_dir + s"${n_best}_ast_fixed_best.txt", fixed_abstract.mkString("\n"))

    write(output_dir + s"${n_best}_ast_buggy_best.txt", buggy_abstract.mkString("\n"))

    write(output_dir + s"${n_best}_ast_predt_best_actions.txt", predt_actions.mkString("\n"))


    results.foreach(wtx => {
      cnt_0 = cnt_0 + wtx.cnt_0
      cnt_1 = cnt_1 + wtx.cnt_1
      cnt_2 = cnt_2 + wtx.cnt_2
      cnt_3 = cnt_3 + wtx.cnt_3
      cnt_4 = cnt_4 + wtx.cnt_4
      cnt_other = cnt_other + wtx.cnt_other  // > 4
      cnt_error = cnt_error + wtx.cnt_error // parse failed
    })


    logger.info(s"[Performance]-[ast]-[${n_best}]-[${fixed_files.size}]\tcounting_[0-4, >4, error]: " +
      s"${cnt_0} ${cnt_1} ${cnt_2} ${cnt_3} ${cnt_4} ${cnt_other} ${cnt_error}")

    val cnt_op_hit = results.map(_.cnt_op_hit).sum / results.length
    val cnt_op_miss = results.map(_.cnt_op_miss).sum / results.length
    val cnt_op_bad = results.map(_.cnt_op_bad).sum / results.length
    val cnt_syntax = results.map(_.cnt_syntax).sum / results.length

    logger.info(s"[Performance]-[ast]-[${n_best}]-[${fixed_files.size}]\tActions {hit, miss, bad}, Syntax Eroor: " +
      f"${cnt_op_hit}%.2f ${cnt_op_miss}%.2f ${cnt_op_bad}%.2f ${cnt_syntax}%.2f")

    val actions_stats = predt_actions.map(docs => docs.split(" ")(1)).groupBy(identity).mapValues(_.length)
    logger.info(s"Actions Statistics: ${actions_stats.toString()}")
  }
}
