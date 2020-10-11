package org.ucf.ml
package parallel


import java.util.concurrent.Callable

import scala.collection.mutable
import java.io.File

import org.ucf.ml.utils.Context

class Worker(wtx: WorkerContext) extends Callable[WorkerContext] with utils.Common {

  val javaPaser = wtx.javaPaser

  def task(buggyPath:String, fixedPath:String, last:Boolean=false) = {
    def _task(ctx:Context, inputPath:String, mode:Value, granularity:Value) = {

      ctx.setCurrentMode(mode)

      if (logger.isDebugEnabled) ctx.append(s"[${wtx.get_work_id}]-${new File(inputPath).getName}")

      val cu = javaPaser.getComplationUnit(inputPath, granularity)

      javaPaser.genAbstractCode(ctx, cu)
    }

    val ctx = new Context(wtx.get_idioms, wtx.get_granularity)

    if ((logger.isDebugEnabled) && (new File(buggyPath).getName != new File(fixedPath).getName)) {
      logger.error(s"[Input]-${buggyPath} != ${fixedPath}")
    }
    _task(ctx, buggyPath, SOURCE, wtx.get_granularity)
    _task(ctx, fixedPath, TARGET, wtx.get_granularity)

    // append results
    wtx.append_buggy(ctx.get_buggy_abstract(wtx.isWithPosition))
    wtx.append_fixed(ctx.get_fixed_abstract(wtx.isWithPosition))

    if (!last) {
      wtx.append_buggy("\n")
      wtx.append_fixed("\n")
    }
  }

  def job(): WorkerContext = {
    val start = System.currentTimeMillis()

    /*Iteration Executing task to handle with all involved in data*/
    for (idx <- 0 until wtx.batch_size) {
      task(wtx.get_src_batch(idx), wtx.get_tgt_batch(idx), idx == wtx.batch_size - 1)
    }
    val stop = System.currentTimeMillis()
    logger.info(f"Worker ${wtx.get_work_id} deal with ${wtx.batch_size} task in ${stop - start} milliseconds")
    wtx
  }
  override def call(): WorkerContext = job()
}
