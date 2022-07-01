package koktai

import scala.io.Source
import scala.util.parsing.json.JSON

/**
  * Created by pierre on 3/27/17.
  */
object PrepareMappings {

  abstract sealed class FontFamily
  case object FM3 extends FontFamily
  case object FK extends FontFamily
  case object NonAstral extends FontFamily
  case object Unknown extends FontFamily


  def loadJson(path:String): Map[String, String] = {
    println(path)
    val src = scala.io.Source.fromFile(path)
    val data = JSON.parseFull(src.getLines() mkString "\n")
      .get
      .asInstanceOf[Map[String, String]]
    src.close()
    data
  }

  def loadDodo(path: String): Map[Int, String] = {
    val src = scala.io.Source.fromFile(path)
    val data = src.getLines()
      .filterNot(_.startsWith("Big5"))
      .map(_.split(","))
      .collect {
        case Array(code, sinogram, user) =>
          val c = code.replace("+", "")
          if (sinogram != "Ⓧ" && c.length < 7) Some(Integer.valueOf(c.toLowerCase,16).toInt -> sinogram.trim)
          else None
      }
      .flatten
      .toMap
    data
  }

  def convertHexaKeys(m: Map[String, String]):Map[Int, String] = {
    m.map { case (k, v) =>
      Integer.valueOf("f" + k, 16).toInt -> v
    }
  }

  def convertStringKeys(m: Map[String, String]):Map[Int, String] = {
    m.map { case (k, v) =>
      val ca = k.toCharArray
      require(Character.codePointCount(ca,0,ca.length) == 1)
      Character.codePointAt(ca,0) -> v
    }
  }



  def readAll():Map[FontFamily, Map[Int, String]] = {
    val m3 = loadJson(getClass.getResource("/Data/m3.json").getPath) // tsu-im
    val k =  loadJson(getClass.getResource("/Data/k.json").getPath)  // tsu-im from k font
    val nonAstral = loadJson(getClass.getResource("/Data/mapping.json").getPath)
    val nonAstral2 = loadDodo(getClass.getResource("/Data/koktai-dodo-all.csv").getPath)  // mapped from k font
    val missings = loadJson(getClass.getResource("/Data/missings.json").getPath)  // still missing after crowdsourcing

    Map(
      FM3 -> convertHexaKeys(m3),
      FK  -> ((convertHexaKeys(k) ++ convertStringKeys(nonAstral)) ++ nonAstral2),
      NonAstral -> (nonAstral2 ++ convertStringKeys(nonAstral)),
      Unknown -> convertHexaKeys(missings)
    )
  }


  def loadIDSMapping(path:String = "/home/pierre/SRCs/koktai/koktai-ids.csv"): Map[String, String] = {
    val src = Source.fromFile(path)
    val RELine = "^B\\+(....),[^,]+,([^,]+),.*".r("code","ids")
    (for (line <- src.getLines().drop(1)) yield {
      line match {
        case RELine(code, ids) => Some(code -> ids)
        case _ =>
          println(s"can't read $line")
          None
      }})
      .flatten
      .toMap
  }

  def decodeRoundedNumber(codePoint: Int): Option[String] = {
    if(0xfc6a1 <= codePoint && codePoint <= 0xfc6a9)
      Some(Character.toChars(0x245f + codePoint - 0xfc6a0) mkString "")
    else
      None
  }

  lazy val mappings: Map[FontFamily, Map[Int,String]] = readAll()
  // todo: gérer les <rt> (tout m3 et quelques fk)
  def astralMapping(font: FontFamily, codepoint : Int): String = {
    val hexString = Integer.toHexString(codepoint)
    codepoint match {
      case c if mappings(font).contains(c) => {
        val out = mappings(font)(c)
        if (font == FM3 || (font == FK && (
          c <= 0xf8df0 ||
            (0xf93a8 <= c && c <= 0xf93c3) ||
            (0xf93c7 <= c && c <= 0xf93cf) ||
            (0xf856c <= c && c <= 0xf856f)
          )))
          s"<rt>$out|$font|$hexString</rt>"
        else s"<mapped>$out|$font|$hexString</mapped>"
      }
      case x =>
        //println(s"$x not in font $font")
        //(mappings(NonAstral).get(x).map(c => s"<mapped>$c|$font|$hexString</mapped>") orElse (
        (if (font == FK && 0xf8cc4 <= x && x <= 0xffefe) // astral of k-font
          Some(s"<missing>${(Character.toChars(x) mkString "") + s"|$font|$hexString"}</missing>")
        //Some(Character.toChars(x) mkString "")
        else
          mappings(Unknown).get(x).map(s => s"<tocheck>$s|$font|$hexString</tocheck>") orElse
            decodeRoundedNumber(x).map(s => s"<mapped>$s|$font|$hexString</mapped>")
          )//) //.getOrElse(s"<missing>${(Character.toChars(x) mkString "") + s"|$x"}</missing>")
          .getOrElse(Character.toChars(x) mkString (""))
    }
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
    aux(streamInput)// not for TEI .replace("</rt>/<rt>", "/")
  }


}
