package org.ucf.ml
package tools

import scala.io.Source
import java.io.FileWriter

object Prediction_Spilt {
  def main(args: Array[String]): Unit = {
    val buggy = Source.fromFile("data/bug-fixes/small/test/buggy.txt").getLines().toList
    val fixed = Source.fromFile("data/bug-fixes/small/test/fixed.txt").getLines().toList
    val pred = Source.fromFile("data/bug-fixes/small/model/pred/predictions.txt").getLines().toList
    val pred_folder = "data/bug-fixes/small/model/pred"

    val total = (buggy, fixed, pred).zipped.toList
    val count_perfect = total.count{case (_, fixed, pred) => pred.trim == fixed.trim}
    val count_bad = total.count{case (buggy, _, pred) => pred.trim == buggy.trim}
    println(s"Prediction: ${total.size}, $count_perfect, $count_bad")

    val pred_right = total.filter{case (_, fixed, pred) => pred.trim == fixed.trim}
    val tgt_right_buggy = new FileWriter(s"${pred_folder}/right/buggy.txt")
    val tgt_right_fixed = new FileWriter(s"${pred_folder}/right/fixed.txt")
    val tgt_right_pred = new FileWriter(s"${pred_folder}/right/pred.txt")
    tgt_right_buggy.write(pred_right.map(_._1).mkString("\n"))
    tgt_right_fixed.write(pred_right.map(_._2).mkString("\n"))
    tgt_right_pred.write(pred_right.map(_._3).mkString("\n"))
    tgt_right_buggy.close()
    tgt_right_fixed.close()
    tgt_right_pred.close()


    val pred_wrong = total.filter{case (_, fixed, pred) => pred.trim != fixed.trim}
    val tgt_wrong_buggy = new FileWriter(s"${pred_folder}/wrong/buggy.txt")
    val tgt_wrong_fixed = new FileWriter(s"${pred_folder}/wrong/fixed.txt")
    val tgt_wrong_pred = new FileWriter(s"${pred_folder}/wrong/pred.txt")

    tgt_wrong_buggy.write(pred_wrong.map(_._1).mkString("\n"))
    tgt_wrong_fixed.write(pred_wrong.map(_._2).mkString("\n"))
    tgt_wrong_pred.write(pred_wrong.map(_._3).mkString("\n"))
    tgt_wrong_buggy.close()
    tgt_wrong_fixed.close()
    tgt_wrong_pred.close()

    println(s"Prediction: ${total.size}, ${pred_right.size}, ${pred_wrong.size}")
  }
}
