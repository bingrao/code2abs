package org.ucf.ml
package parallel


import java.io.{FileNotFoundException, IOException}
import java.util.concurrent.{ExecutorService, Executors}

import scala.collection.JavaConversions._
import net.sourceforge.argparse4j.inf.{Namespace => ConfigNamespace}
import org.ucf.ml.utils.Vocabulary

import scala.jdk.CollectionConverters._

/**
 * https://dzone.com/articles/java-concurrency-multi-threading-with-executorserv
 * https://www.baeldung.com/java-executor-wait-for-threads
 */
class AbstractMaster (config:ConfigNamespace) extends utils.Common {

  /* Load configurations from a file*/
//  val config = new Config(configPath)
  def getConfig = this.config
  private val nums_worker = config.getInt("nums_worker")
  private val is_separated = getConfig.getBoolean("is_separated")
  private val buggy_path = getConfig.getString("buggy_path")
  private val fixed_path = getConfig.getString("fixed_path")
  private val isParallel = nums_worker > 1
  private var pools: ExecutorService = null

  /* Submit workers to executors and start them*/
  def run() = {
    try {
      /* Load buggy and target files, and save their path as a list of string*/
      val (buggy_files, fixed_files) = if (is_separated)
          loadAndCheckData(buggy_path, fixed_path)
        else
          (readFile(buggy_path), readFile(fixed_path))

      // Load data idioms
      val append_vocab = config.getBoolean("append_vocab")
      val idioms_src = new Vocabulary(getConfig).run()
      val idioms_ext = readIdioms(getConfig.getString("idioms_path"))
      val project_idioms = if (append_vocab) idioms_ext.++(idioms_src) else idioms_ext

      val total_files_nums = math.min(buggy_files.size, fixed_files.size)

      // Calcuate nums of files would be processed by a worker
      val batch_size = total_files_nums / nums_worker + 1

      logger.debug(f"The total buggy/fixed files is ${total_files_nums}, " +
        f"#of Workers ${nums_worker}, batch size ${batch_size}")


      // Create workers with allocated data
      val workers  = for (index <- 0 until nums_worker) yield {

        val start = index*batch_size
        val end = if (index == nums_worker - 1) total_files_nums else (index + 1) * batch_size
        logger.info(f"Create a new worker [${index}] to handle Java file indexed from ${start+1} to ${end}")
        new AbstractWorker(new WorkerContext(buggy_batch = buggy_files.slice(start, end),
          fixed_batch = fixed_files.slice(start, end),
          idioms=project_idioms,
          worker_id = index,
          granularity = METHOD,
          batch_size = batch_size,
          isPosition = config.getBoolean("with_position"),
          is_separated = is_separated))
      }

      val results = if (isParallel) {
        // Create a pool of executor computing resources
        pools = Executors.newFixedThreadPool(nums_worker)
        val data = pools.invokeAll(workers) // submit all jobs and wait them finished
        data.map(_.get())
      } else {
        workers.map(_.call())
      }

      val buggy_abstract = results.map(_.get_buggy_abstract).flatMap(_.split("\n")).toList
      val fixed_abstract = results.map(_.get_fixed_abstract).flatMap(_.split("\n")).toList


      if (logger.isDebugEnabled) {
        val buggy_files = buggy_abstract.map(_.split("\t").head)
        val fixed_files = fixed_abstract.map(_.split("\t").head)

        val files = (buggy_files zip fixed_files).filter{ case (src, tgt) => src != tgt }

        if (!files.isEmpty){
          files.foreach {
            case (src, tgt) => logger.error(f"[Ouput]-${src} != ${tgt}")
          }
          System.exit(-1)
        }
      }


//      if (config.getIsSplitData) {
//        import scala.util.Random
//        val random = new Random(100)
//
//        val nums_train = buggy_abstract.size * 0.8
//        val nums_test = buggy_abstract.size * 0.1
//
//        val train_buggy = random.shuffle(buggy_abstract).take(nums_train.toInt)
//        val train_fixed = random.shuffle(fixed_abstract).take(nums_train.toInt)
//        if (config.getIsConcatPosition) {
//          write(getConfig.getOutputBuggyDir + "train-buggy.txt", train_buggy.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "train-fixed.txt", train_fixed.mkString("\n"))
//        } else {
//          val train_buggy_ = train_buggy.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
//          val train_buggy_pos = train_buggy.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}
//          val train_fixed_ = train_fixed.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
//          val train_fixed_pos = train_fixed.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}
//
//          write(getConfig.getOutputBuggyDir + "train-buggy.txt", train_buggy_.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "train-fixed.txt", train_fixed_.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "train-buggy-pos.txt", train_buggy_pos.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "train-fixed-pos.txt", train_fixed_pos.mkString("\n"))
//        }
//
//        val test_buggy = random.shuffle(buggy_abstract diff train_buggy).take(nums_test.toInt)
//        val test_fixed = random.shuffle(fixed_abstract diff train_fixed).take(nums_test.toInt)
//        if (config.getIsConcatPosition) {
//          write(getConfig.getOutputBuggyDir + "test-buggy.txt", test_buggy.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "test-fixed.txt", test_fixed.mkString("\n"))
//        } else {
//          val test_buggy_ = test_buggy.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
//          val test_buggy_pos = test_buggy.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}
//          val test_fixed_ = test_fixed.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
//          val test_fixed_pos = test_fixed.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}
//
//          write(getConfig.getOutputBuggyDir + "test-buggy.txt", test_buggy_.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "test-fixed.txt", test_fixed_.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "test-buggy-pos.txt", test_buggy_pos.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "test-fixed-pos.txt", test_fixed_pos.mkString("\n"))
//        }
//
//        val eval_buggy = buggy_abstract diff train_buggy diff test_buggy
//        val eval_fixed = fixed_abstract diff train_fixed diff test_fixed
//        if (config.getIsConcatPosition) {
//          write(getConfig.getOutputBuggyDir + "eval-buggy.txt", eval_buggy.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "eval-fixed.txt", eval_fixed.mkString("\n"))
//        } else {
//          val eval_buggy_ = eval_buggy.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
//          val eval_buggy_pos = eval_buggy.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}
//          val eval_fixed_ = eval_fixed.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
//          val eval_fixed_pos = eval_fixed.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}
//
//          write(getConfig.getOutputBuggyDir + "eval-buggy.txt", eval_buggy_.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "eval-fixed.txt", eval_fixed_.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "eval-buggy-pos.txt", eval_buggy_pos.mkString("\n"))
//          write(getConfig.getOutputBuggyDir + "eval-fixed-pos.txt", eval_fixed_pos.mkString("\n"))
//        }
//      }

      val buggy_output = getConfig.getString("output_dir") + "total/buggy.txt"
      val fixed_output = getConfig.getString("output_dir") + "total/fixed.txt"

      if (!config.getBoolean("output_position")) {
        write(buggy_output, buggy_abstract.mkString("\n"))
        write(fixed_output, fixed_abstract.mkString("\n"))
      } else {
        val abstract_buggy_ = buggy_abstract.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
        val abstract_buggy_pos = buggy_abstract.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}
        val abstract_fixed_ = fixed_abstract.map{case seq => {seq.split(" ").map(_.split("@").head).mkString(" ")}}
        val abstract_fixed_pos = fixed_abstract.map{case seq => {seq.split(" ").map(_.split("@").last).mkString(" ")}}

        write(getConfig.getString("output_dir") + "total/buggy.txt", abstract_buggy_.mkString("\n"))
        write(getConfig.getString("output_dir") + "total/fixed.txt", abstract_fixed_.mkString("\n"))
        write(getConfig.getString("output_dir") + "total/buggy-pos.txt", abstract_buggy_pos.mkString("\n"))
        write(getConfig.getString("output_dir") + "total/fixed-pos.txt", abstract_fixed_pos.mkString("\n"))
      }

      val abs_conf = Map[String, Object]("run_type" -> "vocabulary",
        "buggy_path"-> buggy_output,
        "fixed_path" -> fixed_output,
        "is_separated" -> false.asInstanceOf[Object],
        "top_k" -> 100000.asInstanceOf[Object]).asJava
      new Vocabulary(new ConfigNamespace(abs_conf)).run()

    } catch  {
      case e: FileNotFoundException => {
        e.printStackTrace()
        println("Couldn't find that file.")
      }
      case e: IOException => {
        e.printStackTrace()
        println("Had an IOException trying to read that file")
      }
      case e:Exception => {
        e.printStackTrace()
      }
    } finally {
      if (pools != null) pools.shutdown()
    }

  }
}

class ASTDiffMaster (config:ConfigNamespace) extends utils.Common {

  /* Load configurations from a file*/
  //  val config = new Config(configPath)
  def getConfig = this.config
  private val nums_worker = getConfig.getInt("nums_worker")
  private val isParallel = nums_worker > 1
  private var pools: ExecutorService = null
  private val n_best = getConfig.getInt("n_best")

  /* Submit workers to executors and start them*/
  def run() = {

    var cnt_0 = 0
    var cnt_1 = 0
    var cnt_2 = 0
    var cnt_3 = 0
    var cnt_4 = 0
    var cnt_error = 0
    var cnt_other = 0

    try {

      /* Load buggy and target files, and save their path as a list of string*/
      val (buggy_files, fixed_files, predt_files) = loadAndCheckData(getConfig.getString("buggy_path"),
        getConfig.getString("fixed_path"),
        getConfig.getString("predt_path"), n_best)


      val total_files_nums = math.min(fixed_files.size, predt_files.size)
      logger.info(s"Loading buggy, fixed  and prdt files ${total_files_nums} with n_best ${n_best}")

      def parallel_collection_run() = {
        import scala.collection.parallel._
        val tasks = mutable.ParArray.tabulate(total_files_nums){ i =>
          val context = new WorkerContext(buggy_files.slice(i, i + 1),
            fixed_files.slice(i, i + 1),
            predt_files.slice(i*n_best, (i+1)*n_best),
            null,
            i,
            n_best,
            METHOD,
            1)
          new ASTDiffWorker(context)
        }
        tasks.map(task => task.call()).toList
      }
      def mutilple_thread_run() = {
        // Calcuate nums of files would be processed by a worker
        val batch_size = total_files_nums / nums_worker + 1

        logger.debug(f"The total buggy/fixed files is ${total_files_nums}, " +
          f"#of Workers ${nums_worker}, batch size ${batch_size}")


        // Create workers with allocated data
        val workers  = for (index <- 0 until nums_worker) yield {

          val start = index*batch_size
          val end = if (index == nums_worker - 1) total_files_nums else (index + 1) * batch_size
          logger.debug(f"Create a new worker [${index}] to handle Java fixed file [${start+1} - ${end}], predt file [${start*n_best+1} - ${end*n_best}]")
          new ASTDiffWorker (new WorkerContext(buggy_batch = buggy_files.slice(start, end),
            fixed_batch = fixed_files.slice(start, end),
            predt_batch = predt_files.slice(start*n_best, end*n_best),
            worker_id = index,
            n_best = n_best,
            granularity = METHOD,
            batch_size = batch_size))
        }


        val results = if (isParallel) {
          // Create a pool of executor computing resources
          pools = Executors.newFixedThreadPool(nums_worker)
          val data = pools.invokeAll(workers) // submit all jobs and wait them finished
          data.map(_.get())
        } else {
          workers.map(_.call())
        }
        results.toList
      }

      val results = mutilple_thread_run()

      val predt_abstract = results.map(_.get_predt_abstract).flatMap(_.split("\n")).toList
      val fixed_abstract = results.map(_.get_fixed_abstract).flatMap(_.split("\n")).toList
      val buggy_abstract = results.map(_.get_buggy_abstract).flatMap(_.split("\n")).toList

      write(getConfig.getString("output_dir") + s"${n_best}_ast_predt_best.txt",
        predt_abstract.mkString("\n"))

      write(getConfig.getString("output_dir") + s"${n_best}_ast_fixed_best.txt",
        fixed_abstract.mkString("\n"))

      write(getConfig.getString("output_dir") + s"${n_best}_ast_buggy_best.txt",
        buggy_abstract.mkString("\n"))

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

    } catch  {
      case e: FileNotFoundException => {
        e.printStackTrace()
        logger.info("Couldn't find that file.")
      }
      case e: IOException => {
        e.printStackTrace()
        logger.info("Had an IOException trying to read that file")
      }
      case e:Exception => {
        e.printStackTrace()
      }
    } finally {
      if (pools != null) pools.shutdown()
    }
  }
}
