
package object koktai {
  abstract sealed class Result

  abstract sealed class PickableData


  val re_markup = "~[a-z0-9]+;".r
  val re_simplert = """<rt>([^<]+)</rt>""".r("content")
  val re_annot = """(.)<rt>([^<]+)</rt>""".r("text", "ruby")

  def cleanForWiki(s: String): String = {
    {re_markup.replaceAllIn(_:String,"")}
      .andThen {re_simplert.replaceAllIn(_, "$1")}
      .apply(s)
  }

  def removeMarkup(s: String): String = {
    re_markup.replaceAllIn(s,"")
  }

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
      case _ => "x"
    }
  }

  abstract sealed class StringResult(val s: String) extends TextResult
  case class Zhuyin(zh: String) extends StringResult(zh)
  case class CJK(c: String) extends StringResult(c)
  case class Ruby(r: String) extends StringResult(r)
  case class Raw(r:String) extends StringResult(r)
  case class SomeChar(c:String) extends StringResult(c)

  implicit def srToStr(sr: StringResult): String = sr.s



  case class Text(content: List[TextResult]) extends TextResult


  case class KokTaiCJK(cjk:String) extends TextResult

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
/*
  object MyPickler {


//    implicit val stringResultPickler = transformPickler({s:String => s.asInstanceOf[StringResult]})(_.asInstanceOf[String])
//    implicit val rubyPickler = transformPickler({s:String => s.asInstanceOf[Ruby]})(_.asInstanceOf[String])
//    implicit val sinoListPickler = transformPickler[List[Sinogram], Seq[Sinogram]](_.toList)(_.toSeq)

    def sinogramToFile(path: String,sino: Sinogram): Unit = {
      val fch = new FileOutputStream(new File(path)).getChannel
      fch.write(Pickle.intoBytes(sino))
      fch.close()

//      val fw = new FileWriter(path)
//      fw.write(write(sino))
//      fw.close()
    }

    def chapterToFile(path:String, chpt: Chapter): Unit = {
      val file = new File(path)
      val fch = new FileOutputStream(file).getChannel
      fch.write(Pickle.intoBytes(chpt))
      fch.close()

//      val fw = new FileWriter(path)
//      fw.write(write(chpt))
//      fw.close()
    }

  } */

}
