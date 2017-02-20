package koktai

import java.io.{File, FileWriter}

import koktai.KokTaiParser.{Chapter, parse}

import scala.io.Source

/**
  * Created by pierre on 2/21/17.
  */
object Main extends App {
  def usage: String =
    """
      |WikiGenerator <ids mapping file> <source file> <destination Directory>
    """.stripMargin

  case class Config(idsMapping: String, src: String, outPath: String)
  def parseArgs(args: List[String], idsMapping: Option[String]= None, src: Option[String]=None, outPath: Option[String]=None): Config = {
    (args, idsMapping, src, outPath) match {
      case (Nil, Some(i), Some(s), Some(o)) => Config(i,s, o)
      case (hd::tl, Some(_), Some(_), None) => parseArgs(tl, idsMapping, src, Some(hd))
      case (hd::tl, Some(_), None, None) => parseArgs(tl, idsMapping, Some(hd))
      case (hd::tl, None, None, None) => parseArgs(tl, Some(hd))
      case _ =>
        println(usage)
        sys.exit(0)
    }
  }

  def loadIDSMapping(path:String = "/home/pierre/SRCs/koktai/koktai-ids.csv"): Map[String, String] = {
    val src = Source.fromFile(path)
    val RELine = "^B\\+(....),[^,]+,([^,]+),.*".r("code","ids")
    (for (line <- src.getLines().drop(1)) yield {
      line match {
        case RELine(code, ids) => Some(code -> s"<ids>$ids</ids>")
        case _ =>
          println(s"can't read $line")
          None
      }})
      .flatten
      .toMap
  }

  def readByChapters(file: String): Seq[String] = {
    val src = Source.fromFile(file)
    src.getLines().foldLeft(List(Nil):List[List[String]])(
      (acc, line) => line match {
        case "" => acc //ignore blank lines
        case ".本文" => acc //ignore
        case ".章首" => List(line.trim)::acc // new chapter
        case _ => (line.trim::acc.head)::acc.tail
      }
    ).reverseMap(_.reverse.mkString("")) // rebuild one string per chapter
  }


  def createWikiFiles(basePath: String, idsMapping: Map[String, String], data: Iterable[String]) = {
    val chapters = data
      .map(parse(KokTaiParser.chapter, _))
      .collect { case KokTaiParser.Success(chpt,_) => chpt}
    for (chpt <- chapters) {
      writeChapter(basePath, chpt, idsMapping)
    }
  }



  //val errors = collection.mutable.HashMap.empty[String, Int].withDefault(_ => 0)

  def writeChapter(basePath: String, chpt: Chapter, idsMapping: Map[String,String]): Unit = {
    val reIDS = "<mark>&#xf(....);</mark>".r("code")

    val file = new File(basePath, chpt.zhuyin.trim.replace("/","|"))
    val fw = new FileWriter(file)
    fw.append(reIDS.replaceAllIn(chpt.toWiki, m => {
      val code = m.group("code")
      if (idsMapping.contains(code))
        idsMapping(code)
      else {
        //    println(code)
    //    errors(code) += 1
        s"(MISSING ${m.group("code")})" //todo: flag wikicode
      }
    }))
   /* println(
      errors
        .toList
        sortBy {c => c._2}
        map { case (c,i) => s"$c $i"}
        mkString "\n")
    println(errors.size) */
    fw.close()
  }


  override def main(argv: Array[String]) = {
    println("hello")
    val config = parseArgs(argv.toList)
    val mapping = loadIDSMapping(config.idsMapping)
    val data = readByChapters(config.src)

    createWikiFiles(config.outPath, mapping, data)

    println("done")
  }

}
