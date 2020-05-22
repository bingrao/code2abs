package org.ucf.ml
package parallel


import java.util.concurrent.Callable
import scala.collection.mutable
import java.io.File

class Worker(workerContext: WorkerContext) extends Callable[WorkerContext] with utils.Common{

  val javaPaser = workerContext.javaPaser

  def task(buggyPath:String, fixedPath:String, last:Boolean=false) = {
    def _task(ctx:Context, inputPath:String, mode:Value, granularity:Value) = {

      ctx.setCurrentMode(mode)

      if (logger.isDebugEnabled) ctx.append(s"[${workerContext.get_work_id}]-${new File(inputPath).getName}")

      val cu = javaPaser.getComplationUnit(inputPath, granularity)

      javaPaser.genAbstractCode(ctx, cu)
    }

    val ctx = new Context(workerContext.get_idioms, workerContext.get_granularity)

    if ((logger.isDebugEnabled) && (new File(buggyPath).getName != new File(fixedPath).getName)) {
      logger.error(s"[Input]-${buggyPath} != ${fixedPath}")
    }
    _task(ctx, buggyPath, SOURCE, workerContext.get_granularity)
    _task(ctx, fixedPath, TARGET, workerContext.get_granularity)

    // append results
    workerContext.append_buggy(ctx.get_buggy_abstract)
    workerContext.append_fixed(ctx.get_fixed_abstract)

    if (!last) {
      workerContext.append_buggy("\n")
      workerContext.append_fixed("\n")
    }
  }

  def job(): WorkerContext = {
    val start = System.currentTimeMillis()

    /*Iteration Executing task to handle with all involved in data*/
    for (idx <- 0 until workerContext.batch_size) {
      task(workerContext.get_src_batch(idx), workerContext.get_tgt_batch(idx), idx == workerContext.batch_size - 1)
    }
    val stop = System.currentTimeMillis()
    logger.info(f"Worker ${workerContext.get_work_id} deal with ${workerContext.batch_size} task in ${stop - start} milliseconds")
    workerContext
  }
  override def call(): WorkerContext = job()
}
