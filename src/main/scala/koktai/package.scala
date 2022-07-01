
package object koktai {
  abstract sealed class Result

  abstract sealed class PickableData


  val re_markup = "~[a-z0-9]+;".r
  val re_simplert = """<rt>([^<]+)</rt>""".r("content")
  val re_annot = """(.)<rt>([^<]+)</rt>""".r("text", "ruby")

  case class Sinogram(cjk: TextResult, annot:Ruby, comment: Option[TextResult], readings: List[Reading],  words: List[Word]) extends Result {

    def titleHasPoint(c: TextResult = cjk): Boolean = c match {
      case SomeChar(c) => c.endsWith("·")
      case Text(l) => !l.isEmpty && titleHasPoint(l.last)
      case _ => false
    }

    def debugCJK = {
      if(titleHasPoint()) {
        cjk match {
          case SomeChar(c) => SomeChar(c.substring(0,c.size - 1))
          case Text(l) => Text(l.take(l.length-1))
          case x => x
        }
      }
      else cjk
    }

    def debugAnnot = {
      if(titleHasPoint()) "·" + annot
      else annot
    }


  }

  case class Chapter(zhuyin: String, pinyin: String, comment: Option[TextResult], sinograms: List[Sinogram], words: List[Word]) extends Result

  abstract sealed class TextResult extends Result {


    override def toString: String = this match {
      case s: StringResult => s.s
      case CJK(c) => c
      case Text(l) => l.map(_.toString) mkString ""
      case KokTaiToCheck(chr, _, _) => chr
      case _ => "x"
    }
  }

  def cleanupTextResult(tr: TextResult): TextResult = tr match {
    case x: Text => cleanupText(x)
    case sr: StringResult => cleanupStringResult(sr)
    case other => other
  }

  abstract sealed class StringResult(val s: String) extends TextResult
  case class Zhuyin(zh: String) extends StringResult(zh)
  case class CJK(c: String) extends StringResult(c)
  case class Ruby(r: String, font: String, code: Int) extends StringResult(r)
  case class Raw(r:String) extends StringResult(r)
  case class SomeChar(c:String) extends StringResult(c)

  implicit def srToStr(sr: StringResult): String = sr.s

  def cleanupStringResult[T <: StringResult](sr: T): T = {
    sr match {
      case Zhuyin(s) => Zhuyin(re_markup.replaceAllIn(s,"")).asInstanceOf[T]
      case CJK(s) => CJK(re_markup.replaceAllIn(s,"")).asInstanceOf[T]
      case Ruby(s, f, c) => Ruby(re_markup.replaceAllIn(s,""), f, c).asInstanceOf[T]
      case Raw(s) => Raw(re_markup.replaceAllIn(s,"")).asInstanceOf[T]
      case SomeChar(s) => SomeChar(re_markup.replaceAllIn(s,"")).asInstanceOf[T]
    }
  }



  case class Text(content: List[TextResult]) extends TextResult

  def cleanupText(t: Text) = Text(t.content.map { x =>
    x match {
      case Zhuyin(s) => Zhuyin(re_markup.replaceAllIn(s, ""))
      case CJK(s) => CJK(re_markup.replaceAllIn(s, ""))
      case Ruby(s, f, c) => Ruby(re_markup.replaceAllIn(s, ""), f, c)
      case Raw(s) => Raw(re_markup.replaceAllIn(s, ""))
      case SomeChar(s) => SomeChar(re_markup.replaceAllIn(s, ""))
      case other => other
    }
  })

  def cleanupString(s: String) = re_markup.replaceAllIn(s,"")


  sealed trait KokTaiNewChar extends TextResult

  case class KokTaiCJK(cjk:String, font: String, code:Int=0, mapped: Boolean=true) extends KokTaiNewChar

  case class KokTaiToCheck(chr: String, font: String, code: Int) extends KokTaiNewChar

  case class CJKRuby(cjk: TextResult, ruby: Ruby) extends TextResult {
    def toWiki: String = s"{{Ruby|${cjk.toString}|${ruby.r.replace("/","<br/>")}}}"

  }

  case class ReadingStart(src: String) extends Result
  case class Reading(src: String, content: TextResult) extends Result {
    def toWiki: String =
      s"""
         |'''$src''' ${content.toString }
       """.stripMargin

  }

  case class Word(title: Text, num: Option[Int], text: Text) extends Result {

    def titleToWiki: String = {
      def rubyToWiki(r:TextResult): String = {
        r match {
          case c: CJKRuby => c.toWiki
          case Text(t) => t map rubyToWiki mkString ""
          case SomeChar(c) => c
        }
      }
       re_markup.replaceAllIn(title.content.map(rubyToWiki) mkString "","")
    }

    def noZhuyin: String = {
      def aux(t:Text): Text = {
        Text(t.content.collect {
          //case Zhuyin(zh: String) =>
          case cjk: CJK => cjk
          //case r:Ruby(r: String, code: Int) extends StringResult(r)
          case CJKRuby(tr,ruby) => tr
          case r: Raw => r
          case c: SomeChar => c
          case txt: Text => aux(txt)
        })
      }
      aux(title).toString
    }

    def onlyZhuyin: String = {
      def aux(t:Text): Text = {
        Text(t.content.collect {
          case Zhuyin(s) => Raw(s)
          case Ruby(r, _, _) => Raw(r)
          case CJKRuby(tr,ruby) => Raw(ruby.r)
          case txt: Text => aux(txt)
        })
      }
      aux(title).toString
    }

    private val posRE = "\\[([^]]+)\\]".r("pos")
    def partOfSpeech: Option[String] =
      posRE
        .findFirstMatchIn(text.toString.take(20))
        .map(_.group("pos"))



    def textToWiki: String = {
      def rubyToWiki(r:TextResult): String = {
        r match {
          case c: CJKRuby => c.toWiki
          case Text(t) => t map rubyToWiki mkString ""
          case SomeChar(c) => c
        }
      }
      re_markup.replaceAllIn((text.content.map(rubyToWiki) mkString ""), "")
    }



    def toWiki: String = {

      s"""
         |==='''【$titleToWiki】'''===
         |:${num.map(i => s"'''$i''' ").getOrElse("")}$textToWiki
       """.stripMargin
    }

  }
}