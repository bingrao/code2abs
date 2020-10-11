package org.ucf.ml.utils

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.stream.Collectors
import scala.collection.mutable
import scala.collection.JavaConversions._


trait Common {

  val logger = new Logging(this.getClass.getName)

  def write(path:String, context:String) = {
    val p = (new File(path)).getParentFile
    val success = if (!p.exists()) p.mkdirs() else true

    if (!success) {
      logger.error(s"The ${path} create failed ...")
      System.exit(-1)
    }

    val printWriter = new PrintWriter(new FileWriter(path))
    printWriter.print(context)
    printWriter.close()
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val allFiles = d.listFiles
      logger.info(s"Get ${allFiles.size} files in directory ${dir}")

      val notFiles = allFiles.filter(!_.isFile)
      notFiles.foreach(file => logger.info("Not a file:" + file.getPath))

      d.listFiles
        .sortBy(_.getName.split("\\.").head.toInt)(ord = Ordering[Int])
        .filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  /*load Data idioms from existing file*/
  def readIdioms(filePath:String = "data/idioms/idioms.csv") = {
    var idioms = new mutable.HashSet[String]()
    try{
      val stream = Files.lines(Paths.get(filePath))
      idioms.++=(stream.collect(Collectors.toSet[String]()))
    } catch {
      case e:Exception => e.printStackTrace()
    }
    idioms
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
