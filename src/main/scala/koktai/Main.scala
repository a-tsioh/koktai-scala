package koktai

import java.io.{File, FileWriter}

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


  def createWikiFiles(basePath: String, idsMapping: Option[Map[String, String]], data: Iterable[String]) = {
    val chapters = data
      .map(parse(KokTaiParser.chapter, _))
      .collect { case KokTaiParser.Success(chpt,_) => chpt}
    for (chpt <- chapters) {
      writeChapter(basePath, chpt, idsMapping)
    }
  }

   def createHtmlFiles(basePath: String, idsMapping: Option[Map[String, String]], data: Iterable[String]) = {
     def writeOne(basePath: String, chpt: Chapter): Unit = {

       val filename =
        if(chpt.zhuyin.trim.isEmpty){
           println("empty filename")
           println(chpt.pinyin)
            "lastEmpty.html"
        }
        else chpt.zhuyin.trim.replace("/", "|")
       val file = new File(basePath, filename + ".html" )
       val fw = new FileWriter(file)
       fw.write(chpt.toHtmlSinglePage.buildString(true))
       fw.close()
     }
    val chapters = data
      .map(convertAstralChars)
      .map(parse(KokTaiParser.chapter, _))
      .collect { case KokTaiParser.Success(chpt,_) => chpt}
    for (chpt <- chapters) {
      writeOne(basePath, chpt)
    }
  }

  def createHtmlTree(basePath: String, data: Iterable[String]) = {



    def writeOneSinogram(path:String, i: Int, sino: Sinogram): Unit = {
      val filename = s"$i.html"
      val fw = new FileWriter(new File(path, filename ))
      fw.write(sino.toHTMLShortPage.buildString(true))
      fw.close()
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
      val file = new File(path, s"$i.html" )
      val fw = new FileWriter(file)
      fw.write(chpt.toHtmlShortPage(i).buildString(true))
      fw.close()
      new File(path, i.toString).mkdir()
      (name -> i)
    }


    val chapters = data.par
      .map(convertAstralChars)
      .map(parse(KokTaiParser.chapter, _))
      .collect { case KokTaiParser.Success(chpt,_) => chpt}
      .seq.zipWithIndex
    val indexMapping = (for((chpt, count) <- chapters.par) yield {
      val idx = writeOneChapter(basePath, count,chpt)
      chpt.sinograms.zipWithIndex.foreach {case (s,i) => writeOneSinogram(s"$basePath/$count/", i, s) }
      idx
    }).seq.toMap

    // writing index
    val indexPage =
      <html  lang="zh-Hant" class="han-init">
      {HtmlHeaders}
        <body  lang="zh-Hant">
        {indexMapping.map{case (s,i) => <a href={s"$i.html"}>{s}</a><br/>}}
          <script src="https://cdnjs.cloudflare.com/ajax/libs/Han/3.3.0/han.min.js"></script>
        </body>
      </html>.buildString(true)

    val fw = new FileWriter(new File(basePath, "index.html"))
    fw.write(indexPage)
    fw.close
  }



  //val errors = collection.mutable.HashMap.empty[String, Int].withDefault(_ => 0)

  def writeChapter(basePath: String, chpt: Chapter, idsMapping: Option[Map[String,String]]): Unit = {
    val reMark = "<mark>&#xf(....);</mark>".r("code")
    val reIMG = "<img ?src=\\\"img/(m3|k)/([^\\\"]+).png\\\" ?/>".r("dir", "code")

    val file = new File(basePath, chpt.zhuyin.trim.replace("/","|"))
    val fw = new FileWriter(file)
    val txt = idsMapping.map({ ids =>
      val unMarked = reMark.replaceAllIn(chpt.toWiki, m => {
          val code = m.group("code")
          if (ids.contains(code))
            ids(code)
          else {
            //    println(code)
            //    errors(code) += 1
            s"{{MissingIDS|$code}}" //todo: flag wikicode
          }
        })
        reIMG.replaceAllIn(unMarked, m => {
          val dir = m.group("dir")
          val code = m.group("code")
          if (dir == "k" && ids.contains(code))
            ids(code)
          else
            s"{{MissingIDS|$dir/$code}}"
        })
      })
      .getOrElse({
        val unmarked = reMark.replaceAllIn(chpt.toWiki, m => {
        val code = m.group("code")
        //s"""<img src="img/k/$code.png" alt="$code" />"""
        s"""<img src="https://commons.wikimedia.org/wiki/File:Koktai_dictionary_missing_char_k_$code.png" alt="k/$code.png"/>"""
        })
        reIMG.replaceAllIn(unmarked, m => {
          val dir = m.group("dir")
          val code = m.group("code")
          s"""<img src="https://commons.wikimedia.org/wiki/File:Koktai_dictionary_missing_char_${dir}_$code.png" alt="$dir/$code.png"/>"""

        })

      }
      )
    fw.append(txt)
    fw.close()
  }


  override def main(argv: Array[String]) = {
    println("hello " + (argv.mkString(", ")))
    mappings = PrepareMappings.readAll()
    val config = parseArgs(argv.toList)
    val mapping = config.idsMapping.map(loadIDSMapping)
    val data = readByChapters(config.src.get)

    //  createWikiFiles(config.outPath.get, mapping, data)
    //createHtmlFiles(config.outPath.get, mapping, data)
    createHtmlTree(config.outPath.get, data)
    println("done")
  }

}
