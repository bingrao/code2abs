package org.ucf.ml
package parallel


import java.util.concurrent.Callable
import java.io.File
import org.ucf.ml.utils.Context

class AbstractWorker(wtx: WorkerContext) extends Callable[WorkerContext] with utils.Common {

  val javaPaser = wtx.javaPaser

  def task(buggyPath:String, fixedPath:String, last:Boolean=false) = {
    def _task(ctx:Context, inputPath:String, mode:Value, granularity:Value) = {
      try {
        ctx.setCurrentMode(mode)
        if (logger.isDebugEnabled) ctx.append(s"[${wtx.get_work_id}]-${new File(inputPath).getName}")

        val cu = javaPaser.getBPEComplationUnit(inputPath, ctx, granularity)

        javaPaser.genAbstractCode(ctx, cu)
      } catch {
        case e: Exception => {
          logger.info(s"The working model: ${mode}, error input ${inputPath}")
          e.printStackTrace()
        }
      }
    }

    val ctx = new Context(wtx.get_idioms, wtx.get_granularity)

    if ((logger.isDebugEnabled) && (new File(buggyPath).getName != new File(fixedPath).getName)) {
      logger.error(s"[Input]-${buggyPath} != ${fixedPath}")
    }

    _task(ctx, buggyPath, SOURCE, wtx.get_granularity)
    _task(ctx, fixedPath, TARGET, wtx.get_granularity)

    // append results
    val buggy_abstract = ctx.get_buggy_abstract(wtx.isWithPosition)
    val fixed_abstract = ctx.get_fixed_abstract(wtx.isWithPosition)

    if (buggy_abstract.contains("\n") || fixed_abstract.contains("\n"))
      logger.info(s"Error -> Buggy_Path: ${buggyPath}, Fixed_Path: ${fixedPath}")

    wtx.append_buggy(buggy_abstract)
    wtx.append_fixed(fixed_abstract)

    if (!last) {
      wtx.append_buggy("\n")
      wtx.append_fixed("\n")
    }
  }

  def job(): WorkerContext = {
    val start = System.currentTimeMillis()

    /*Iteration Executing task to handle with all involved in data*/
    for (idx <- 0 until wtx.size) {
      task(wtx.get_buggy_batch(idx), wtx.get_fixed_batch(idx), idx == wtx.size - 1)
    }
    val stop = System.currentTimeMillis()
    logger.info(f"Worker ${wtx.get_work_id} deal with ${wtx.size} task in ${(stop - start)/1000} seconds")
    wtx
  }
  override def call(): WorkerContext = job()
}

class ASTDiffWorker(wtx: WorkerContext) extends Callable[WorkerContext] with utils.Common {

  val javaPaser = wtx.javaPaser

  def get_context = wtx

  def task(buggyPath:String, fixedPath:String, predtPath:List[String], idx:Int): Unit = {

    val best_match_count = predtPath.map { predt =>
      val results = try {
        javaPaser.getComplationUnit(predt, METHOD, false)
        Right(javaPaser.getASTDiffCount(fixedPath, predt, isFile = false))
      } catch {
        case e: Exception => Left(e)
      }
      results match {
        case Right(r) => r
        case Left(_) => Int.MaxValue
      }
    }
    val min_count = best_match_count.min

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
    wtx.append_predt(s"#${wtx.get_batch_size*wtx.get_work_id + idx}#${prefix}#\t" + predtPath(best_match_count.indexOf(min_count)))
    wtx.append_fixed(s"#${wtx.get_batch_size*wtx.get_work_id + idx}#${prefix}#\t" + fixedPath)
    wtx.append_buggy(s"#${wtx.get_batch_size*wtx.get_work_id + idx}#${prefix}#\t" + buggyPath)

    if (idx != wtx.size - 1) {
      wtx.append_predt("\n")
      wtx.append_fixed("\n")
      wtx.append_buggy("\n")
    }
  }

  def job(): WorkerContext = {
    val start = System.currentTimeMillis()

    /*Iteration Executing task to handle with all involved in data*/
    for (idx <- 0 until wtx.size) {
      task(wtx.get_buggy_batch(idx), wtx.get_fixed_batch(idx), wtx.get_predt_batch(idx), idx)
    }
    val stop = System.currentTimeMillis()
    logger.debug(f"Worker ${wtx.get_work_id} deal with ${wtx.size} task in ${(stop - start) / 10000 } seconds")
    wtx
  }
  override def call(): WorkerContext = job()
}
