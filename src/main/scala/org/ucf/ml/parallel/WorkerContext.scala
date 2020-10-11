package org.ucf.ml
package parallel
import org.ucf.ml.utils.Config

import scala.collection.mutable

/**
 *
 * @param src_batch, a list of buggy (source) input files' path
 * @param tgt_batch, a list of fixed (target) input files' path
 * @param idioms, a bag of idioms vocabulary keep by this project
 * @param worker_id, integer id assigned by master
 */
class WorkerContext(src_batch:List[String] = null,
                    tgt_batch:List[String] = null,
                    idioms:mutable.HashSet[String],
                    worker_id:Int,
                    granularity: Value = METHOD,
                    config: Config) {

  private val buggy_abstract = new StringBuilder
  private val fixed_abstract = new StringBuilder

  def append_buggy(content:String) = this.buggy_abstract.append(content)
  def append_fixed(content:String) = this.fixed_abstract.append(content)

  def get_buggy_abstract = buggy_abstract.toString()
  def get_fixed_abstract = fixed_abstract.toString()

  val javaPaser = new parser.JavaParser
  val batch_size = scala.math.min(src_batch.size, tgt_batch.size)

  def get_idioms = this.idioms
  def get_src_batch = this.src_batch
  def get_tgt_batch = this.tgt_batch
  def get_work_id = this.worker_id
  def get_granularity = this.granularity

  def isWithPosition = config.getIsWithPosition

}
