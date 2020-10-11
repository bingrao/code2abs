package org.ucf.ml
package parallel


import java.io.{FileNotFoundException, IOException}
import java.util.concurrent.{ExecutorService, Executors}

import org.ucf.ml.utils.Config

import scala.collection.JavaConversions._
import net.sourceforge.argparse4j.inf.{Namespace => ConfigNamespace}

/**
 * https://dzone.com/articles/java-concurrency-multi-threading-with-executorserv
 * https://www.baeldung.com/java-executor-wait-for-threads
 */
class Master (config:ConfigNamespace) extends utils.Common {

  /* Load configurations from a file*/
//  val config = new Config(configPath)
  def getConfig = this.config
  private val nums_worker = config.getInt("nums_worker")
  private val isParallel = nums_worker > 1
  private var pools: ExecutorService = null

  /* Submit workers to executors and start them*/
  def run() = {
    try {

      // Load data idioms
      val project_idioms = readIdioms(getConfig.getString("idioms_path"))

      /* Load buggy and target files, and save their path as a list of string*/
      val (buggy_files, fixed_files) = loadAndCheckData(getConfig.getString("buggy_path"),
        getConfig.getString("fixed_path"))


      val total_files_nums = math.min(buggy_files.size, fixed_files.size)


      // Calcuate nums of files would be processed by a worker
      val batch_size = total_files_nums / nums_worker + 1

      logger.info(f"The total buggy/fixed files is ${total_files_nums}, " +
        f"#of Workers ${nums_worker}, batch size ${batch_size}")


      // Create workers with allocated data
      val workers  = for (index <- 0 until nums_worker) yield {

        val start = index*batch_size
        val end = if (index == nums_worker - 1) total_files_nums else (index + 1) * batch_size
        logger.info(f"Create a new worker [${index}] to handle Java file indexed from ${start+1} to ${end}")
        new Worker(new WorkerContext(src_batch = buggy_files.slice(start, end),
          tgt_batch = fixed_files.slice(start, end),
          idioms=project_idioms,
          worker_id = index,
          granularity = METHOD,
          config))
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

      if (!config.getBoolean("output_position")) {
        write(getConfig.getString("output_dir") + "total/buggy.txt", buggy_abstract.mkString("\n"))
        write(getConfig.getString("output_dir") + "total/fixed.txt", fixed_abstract.mkString("\n"))
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

  /*################################# Helper Functions #####################################*/
  def loadAndCheckData(srcPath:String, tgtPath:String):(List[String], List[String]) = {
    val srcFiles = getListOfFiles(srcPath)
    val tgtFiles = getListOfFiles(tgtPath)

    logger.info(f"Loading ${srcFiles.size} java files from ${srcPath}")
    logger.info(f"Loading ${tgtFiles.size} java files from ${tgtPath}")

    if (srcFiles.size != tgtFiles.size){
      logger.error(f"The sizes of source (${srcFiles.size}) and target (${tgtFiles}) do not match ...")
      System.exit(-1)
    }

    val files = (srcFiles zip tgtFiles).filter{
      case (src, tgt) => src.getName != tgt.getName}

    if (files.size != 0){
      files.foreach{case (src, tgt) => logger.error(f"[Check]-${src} != ${tgt}")}
      System.exit(-1)
    }

    (srcFiles.map(_.getPath), tgtFiles.map(_.getPath))
  }
}
