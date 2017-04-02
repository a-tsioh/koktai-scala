/**
  * Created by pierre on 4/2/17.
  */
package object koktai {
  abstract sealed class Result

  val re_markup = "~[a-z0-9]+;".r
  val re_simplert = """<rt>([^<]+)</rt>""".r("content")
  val re_annot = """(.)<rt>([^<]+)</rt>""".r("text", "ruby")

  def cleanForWiki(s: String): String = {
    {re_markup.replaceAllIn(_:String,"")}
      .andThen {re_simplert.replaceAllIn(_, "$1")}
      .apply(s)
  }




  case class Sinogram(cjk: String, annot:Ruby, comment: String, readings: List[Reading],  words: List[Word]) extends Result {
    def toWiki: String = {
      s"""
         |=={{ruby|$cjk|${annot}}}==
         |$comment
         |${readings.map(_.toWiki) mkString ""}
         |
         |${words.map(_.toWiki) mkString ""}
       """.stripMargin
    }

    def toHTML =
      <div>
        <h2><ruby>{cjk}<rt>{annot}</rt></ruby></h2>
        <div>{comment}</div>
        <div>{readings.map(_.toHTML)}</div>
        <div>{words.map(_.toHTML)}</div>
      </div>


  }

  case class Chapter(zhuyin: String, pinyin: String, comment: String, sinograms: List[Sinogram], words: List[Word]) extends Result {
    def toWiki: String = {
      s"""
         |= $zhuyin [$pinyin] =
         |$comment
         |${words map {_.toWiki} mkString "\n"}
         |${sinograms map {_.toWiki} mkString "\n"}
       """.stripMargin
    }

    def toHTML =
      <html lang="zh-Hant-TW">
        <head>
          <meta charset="utf8" />
          <script src="han.min.js"></script>
          <link rel="stylesheet" media="all" href="style.css" />
        </head>
        <body>
          <h1>{s"$zhuyin [$pinyin]"}</h1>
          <div>{comment}</div>
          {words map {_.toHTML}}
          {sinograms map {_.toHTML}}
        </body>
      </html>
  }


  abstract sealed class TextResult extends Result {
    def toHtml: scala.xml.Node = this match  {
      case c: CJKRuby => c.toHTML //<ruby>{c.cjk}<rt>{c.ruby}</rt></ruby>
      case Text(t) => <span>{t map {_.toHtml}}</span>
      case SomeChar(c) => scala.xml.Text(c)
      case c: KokTaiCJK => c.toHtml
      case s: StringResult => scala.xml.Text(s.asInstanceOf[String])
      case CJK(c) => scala.xml.Text(c)
    }

    override def toString = this match {
      case s: StringResult => s.asInstanceOf[String]
      case CJK(c) => c
      case x => super.toString
    }
  }

  abstract class StringResult extends TextResult
  type Zhuyin = String with StringResult
  case class CJK(c: String) extends TextResult
  type Ruby = String with StringResult
  type Raw = String with StringResult

  case class Text(content: List[TextResult]) extends TextResult
  case class SomeChar(c:String) extends TextResult

  case class KokTaiCJK(cjk:String) extends TextResult {
    override def toHtml = <mark>{cjk}</mark>
  }

  case class CJKRuby(cjk: TextResult, ruby: String) extends TextResult {
    def toWiki: String = s"{{Ruby|${cjk.toString}|${ruby.replace("/","<br/>")}}}"

    def multiRT(ruby: String): scala.xml.Node = {
      def aux(l: List[String], acc:List[scala.xml.Node]): scala.xml.Node = {
        l match {
          case Nil => <rt>{acc.reverse}</rt>
          case List(single) => aux(Nil, scala.xml.Text(single):: acc)
          case hd::tl => aux(tl, <br/> :: scala.xml.Text(hd) :: acc)
        }
      }
      aux(ruby.split("/").toList, Nil)
    }

    def toHTML = <ruby>{cjk.toHtml}{multiRT(ruby)}</ruby>
  }

  case class ReadingStart(src: String) extends Result
  case class Reading(src: String, content: String) extends Result {
    def toWiki: String =
      s"""
         |'''$src''' ${cleanForWiki(content) }
       """.stripMargin

    def toHTML =
      <div>
        <b>{src}</b> {cleanForWiki(content)}
      </div>
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

    def titleToHtml = {
      <h3>{title.content.map {_.toHtml}}</h3>
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

    def textToHtml = {
      <span>
        {text.content map {_.toHtml}}
      </span>

    }

    def toWiki: String = {

      s"""
         |==='''【$titleToWiki】'''===
         |:${num.map(i => s"'''$i''' ").getOrElse("")}$textToWiki
       """.stripMargin
    }

    def toHTML: scala.xml.Elem =
      <div>
        {titleToHtml}
        <div>
          {num.map({case i => <b>{i.toString}</b>}).getOrElse("")}{textToHtml}
        </div>
      </div>
  }
}
