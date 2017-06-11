package koktai

import java.io.{File, FileOutputStream, FileReader, FileWriter}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import koktai.KokTaiParser.parse

import scala.io.Source



/**
  * Created by pierre on 2/21/17.
  */
object Main extends App {

  def usage: String =
    """
      |WikiGenerator <ids mapping file> <source file> <destination Directory>
    """.stripMargin

  case class Config(src: Option[String], outPath: Option[String], idsMapping: Option[String])
  def parseArgs(args: List[String], config: Config = Config(None, None, None)): Config = {
    args match {
      case Nil =>
        if(config.src.isEmpty || config.outPath.isEmpty){
          println(usage)
          sys.exit(0)
        }
        else config
      case path::tl =>
        config match {
          case Config(None, _, _) => parseArgs(tl, config.copy(src = Some(path)))
          case Config(_, None, _) => parseArgs(tl,config.copy(outPath = Some(path)))
          case Config(_, _, None) => parseArgs(tl,config.copy(idsMapping = Some(path)))
          case _ =>
            println(usage)
            sys.exit(0)
        }
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

  abstract sealed class FontFamily
  case object FM3 extends FontFamily
  case object FK extends FontFamily
  case object NonAstral extends FontFamily
  case object Unknown extends FontFamily


  var mappings:Map[FontFamily,Map[Int,String]] = PrepareMappings.readAll()

  def decodeRoundedNumber(codePoint: Int): Option[String] = {
    if(0xfc6a1 <= codePoint && codePoint <= 0xfc6a9)
      Some(Character.toChars(0x245f + codePoint - 0xfc6a0) mkString "")
    else
      None
  }

  // todo: gérer les <rt> (tout m3 et quelques fk)
  def astralMapping(font: FontFamily, codepoint : Int): String = codepoint match {
    case c if mappings(font).contains(c) => {
      val out = mappings(font)(c)
      if(font == FM3 || (font == FK && (
        c <= 0xf8df0 ||
          (0xf93a8 <= c && c <= 0xf93c3 ) ||
          (0xf93c7 <= c && c <= 0xf93cf) ||
          (0xf856c <= c && c <= 0xf856f)
      )))
        s"<rt>$out</rt>"
      else s"<mark>$out</mark>"
    }
    case x =>
      val str = Character.toChars(x) mkString ""
      val out: String =
        if(font == FK && 0xf8cc4  <= x && x <= 0xffefe )
          s"<mark>$str</mark>"
        else
          (mappings(Unknown).get(x) orElse
            mappings(NonAstral).get(x) orElse
            decodeRoundedNumber(x)).getOrElse(str)
      out
  }

  def convertAstralChars(input:String): String = {
    val streamInput = input.toStream
    def aux(in: Stream[Char],out:List[String] = Nil, currentFont: FontFamily = FM3): String = {
      in match {
        case Stream.Empty => out.reverse mkString ""
        case '~' #:: 'f' #:: 'k' #:: tl => aux(tl, "~fk"::out, FK)
        case '~' #:: 'f' #:: 'm' #:: '3' #:: tl => aux(tl, "~fm3" :: out, FM3)
        case hs #:: ls #:: tl if hs.isHighSurrogate && ls.isLowSurrogate => aux(tl, astralMapping(currentFont, Character.codePointAt(Array(hs,ls),0))::out, currentFont)
        case c #:: tl => aux(tl, astralMapping(currentFont, c.toInt)::out, currentFont)
      }
    }
    aux(streamInput).replace("</rt>/<rt>", "/")
  }

  def readByChapters(file: String): Seq[String] = {
    val reChar = "^~(fm7|fk|fm3)t168".r
    val src = Source.fromFile(file)
    src.getLines().foldLeft(List(Nil):List[List[String]])(
      (acc, line) => line match {
        case "" => acc //ignore blank lines
        case ".本文" => acc //ignore
        case ".章首" => List(line.trim)::acc // new chapter
        case _ =>
          val l = reChar.findFirstIn(line).map(_ => "<CHAR/>" + line).getOrElse(line)
          (l.trim::acc.head)::acc.tail
      }
    ).reverseMap(_.reverse.mkString("")) // rebuild one string per chapter
  }


  def createPicklesTree(basePath: String, data: Iterable[String]) = {

    def writeOneSinogram(path: String, chpt: Int, i: Int, sino: Sinogram): Unit = {
      val filename = s"sino-$chpt-$i.pkl"
      sinogramPickler.toFile(filename, sino)
    }

    def writeOneChapter(path:String, i: Int, chpt: Chapter): (String, Int) = {
      val name =
        if(chpt.zhuyin.trim.isEmpty){
          println("empty filename")
          println(chpt.pinyin)
          "lastEmpty.html"
        }
        else chpt.zhuyin.trim
      //indexMapping.append(name -> i)
      val file = new File(path, s"chpt-$i.pkl" )
      chapterPickler.toFile(file.getAbsolutePath, (i,chpt))
      name -> i
    }

    val chapters = data.par
      .map(convertAstralChars)
      .map(parse(KokTaiParser.chapter, _))
      .collect { case KokTaiParser.Success(chpt,_) => chpt}
      .seq.zipWithIndex
    val indexMapping = (for((chpt, count) <- chapters.par) yield {
      val idx = writeOneChapter(basePath, count,chpt)
      chpt.sinograms.zipWithIndex.foreach {case (s,i) => writeOneSinogram(s"$basePath/",count, i, s) }
      idx
    }).seq

    val initialsMapping = indexMapping.foldLeft(Map.empty[String, Map[String, Int]].withDefaultValue(Map.empty)) {case (m,(syl,i)) =>
      val firstLetter = syl.substring(0,1)
        m + (firstLetter -> (m(firstLetter) + (syl -> i)))
    }

    val idxFile = new File(basePath, "index.pkl")
    indexPickler.toFile(idxFile.getAbsolutePath, initialsMapping)
  }


  override def main(argv: Array[String]) = {
    import collection.JavaConverters._
    import boopickle.Default._
    println("hello " + (argv.mkString(", ")))
    //val buf = ByteBuffer.wrap(Source.fromFile("/tmp/pickles/sino-1-1.pkl").mkString("").getBytes(Charset.))
    //println(Unpickle[Sinogram].fromBytes(buf))
    //println(sinogramPickler.ofFile("/tmp/pickles/sino-1-1.pkl"))
    //val str = Files.readAllLines(Paths.get("/tmp/pickles/sino-1-1.pkl")).asScala.mkString("\n")
    //println(str)
//    val ba = str.map(_.toByte).toArray
//    println(ba)
//    println(Unpickle[Sinogram].fromBytes(ByteBuffer.wrap(ba)))
    mappings = PrepareMappings.readAll()
    val config = parseArgs(argv.toList)
    val mapping = config.idsMapping.map(loadIDSMapping)
    val data = readByChapters(config.src.get)

    //  createWikiFiles(config.outPath.get, mapping, data)
    //createHtmlFiles(config.outPath.get, mapping, data)
    //createHtmlTree(config.outPath.get, data)
    createPicklesTree(config.outPath.get, data)
    println("done")
  }

}
