package org.ucf.ml
package parallel
import java.lang
import scala.collection.mutable

/**
 *
 * @param buggy_batch, a list of buggy (source) input files' path
 * @param fixed_batch, a list of fixed (target) input files' path
 * @param idioms, a bag of idioms vocabulary keep by this project
 * @param worker_id, integer id assigned by master
 */
class WorkerContext(buggy_batch:List[String],
                    fixed_batch:List[String],
                    predt_batch:List[String] = null,
                    idioms:mutable.HashSet[String] = null,
                    worker_id:Int,
                    n_best:Int = 1,
                    granularity: Value = METHOD,
                    batch_size:Int,
                    isPosition:Boolean=false,
                    _isFile:Boolean=true) extends Serializable {

  private val buggy_abstract = new StringBuilder
  private val buggy_abstract_path = new StringBuilder

  private val fixed_abstract = new StringBuilder
  private val fixed_abstract_path = new StringBuilder

  private val predt_abstract = new StringBuilder


  def append_buggy(content:String): mutable.StringBuilder = this.buggy_abstract.append(content)
  def append_buggy_path(content:String): mutable.StringBuilder = this.buggy_abstract_path.append(content)

  def append_fixed(content:String): mutable.StringBuilder = this.fixed_abstract.append(content)
  def append_fixed_path(content:String): mutable.StringBuilder = this.fixed_abstract_path.append(content)
  def append_predt(content:String): mutable.StringBuilder = this.predt_abstract.append(content)

  def get_buggy_abstract:String = buggy_abstract.toString()
  def get_buggy_abstract_path:String = buggy_abstract_path.toString()

  def get_fixed_abstract:String = fixed_abstract.toString()
  def get_fixed_abstract_path:String = fixed_abstract_path.toString()

  def get_predt_abstract:String = predt_abstract.toString()

  val javaPaser = new parser.JavaParser
  def get_batch_size: Int = batch_size

  def size:Int = scala.math.min(fixed_batch.size, buggy_batch.size)

  def get_idioms:mutable.HashSet[String] = this.idioms

  def get_buggy_batch(index:Int):String = this.buggy_batch(index)
  def get_fixed_batch(index:Int):String = this.fixed_batch(index)
  def get_predt_batch(index:Int): List[String] = this.predt_batch.slice(index*n_best, (index+1)*n_best)

  def get_work_id:Int = this.worker_id
  def get_granularity:Value = this.granularity
  def get_n_best:Int = n_best

  var cnt_0 = 0
  var cnt_1 = 0
  var cnt_2 = 0
  var cnt_3 = 0
  var cnt_4 = 0
  var cnt_error = 0
  var cnt_other = 0

//  def isWithPosition: lang.Boolean = config.getBoolean("with_position")
  def isWithPosition: lang.Boolean = isPosition

  def isFile: lang.Boolean = _isFile
}
