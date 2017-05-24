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

  def removeMarkup(s: String): String = {
    re_markup.replaceAllIn(s,"")
  }

  // <script src="han.min.js"></script>
  val HtmlHeaders =
    <head>
      <meta charset="utf8" />
      <link rel="stylesheet" media="all" href="/koktai-scala/style.css" />
    </head>
//</link>https://cdnjs.cloudflare.com/ajax/libs/Han/3.3.0/han.min.css" />
  case class Sinogram(cjk: TextResult, annot:Ruby, comment: Option[TextResult], readings: List[Reading],  words: List[Word]) extends Result {
    def toWiki: String = {
      s"""
         |=={{ruby|$cjk|${annot}}}==
         |${comment.map(_.toHtml).getOrElse("")}
         |${readings.map(_.toWiki) mkString ""}
         |
         |${words.map(_.toWiki) mkString ""}
       """.stripMargin
    }

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


    def toHTMLShortPage =
      <html lang="zh-Hant" class="han-init">
        {HtmlHeaders}
        <body lang="zh-Hant">
          <div class="sinogram">
            <h2><ruby>{debugCJK.toHtml}<rt>{debugAnnot}</rt></ruby></h2>
            <div class="comment">{comment.map{_.toHtml}.getOrElse("")}</div>
            <div class="readings">{readings.map(_.toHTML)}</div>
            <div class="words">{words.map(_.toHTML)}</div>
          </div>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/Han/3.3.0/han.min.js"></script>
        </body>
      </html>



    def toHTML =
      <div class="sinogram">
        <h2><ruby>{debugCJK.toHtml}<rt>{debugAnnot}</rt></ruby></h2>
        <div class="comment">{comment.map{_.toHtml}.getOrElse("")}</div>
        <div class="readings">{readings.map(_.toHTML)}</div>
        <div class="words">{words.map(_.toHTML)}</div>
      </div>


  }

  case class Chapter(zhuyin: String, pinyin: String, comment: Option[TextResult], sinograms: List[Sinogram], words: List[Word]) extends Result {
    def toWiki: String = {
      s"""
         |= $zhuyin [$pinyin] =
         |$comment
         |${words map {_.toWiki} mkString "\n"}
         |${sinograms map {_.toWiki} mkString "\n"}
       """.stripMargin
    }

    def toHtmlShortPage(i0: Int) = {
      <html lang="zh-Hant-TW">
        {HtmlHeaders}
        <body>
      <h1>{s"$zhuyin ${removeMarkup(pinyin)}"}</h1>
        <div class="chpt-comment">{comment.map {_.toHtml}.getOrElse("")}</div>
      {words map {_.toHTML}}
      <h3>{sinograms.zipWithIndex map {case (s,i) =>  <a href={s"./$i0/$i.html"}>{s.cjk.toHtml}</a>}}</h3>
        </body>
      </html>
    }

    def toHtmlSinglePage =
      <html lang="zh-Hant-TW">
        {HtmlHeaders}
        <body>
          <h1>{s"$zhuyin ${removeMarkup(pinyin)}"}</h1>
          <div class="chpt-comment">{comment.map {_.toHtml}.getOrElse("")}</div>
          {words map {_.toHTML}}
          {sinograms map {_.toHTML}}
        </body>
      </html>
  }


  abstract sealed class TextResult extends Result {
    def toHtml: scala.xml.Node = this match  {
      case c: CJKRuby => c.toHTML //<ruby>{c.cjk}<rt>{c.ruby}</rt></ruby>
      case Text(t) => <span>{t map {_.toHtml}}</span>
      case SomeChar(c) => scala.xml.Text(removeMarkup(c))
      case c: KokTaiCJK => c.toHtml
      case s: StringResult => scala.xml.Text(removeMarkup(s.asInstanceOf[String]))
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
    override def toHtml = <em class="k">{cjk}</em>
  }

  case class CJKRuby(cjk: TextResult, ruby: String) extends TextResult {
    def toWiki: String = s"{{Ruby|${cjk.toString}|${ruby.replace("/","<br/>")}}}"
    def multiRT(ruby: String):scala.xml.NodeSeq = {
      def aux(l: List[String], acc:List[scala.xml.Node]): scala.xml.NodeSeq = {
        l match {
          case Nil => {acc.reverse}.map {t => <rtc><rt>{t}</rt></rtc>}
          case List(single) => aux(Nil, scala.xml.Text(removeMarkup(single)):: acc)
          case hd::tl => aux(tl, scala.xml.Text(removeMarkup(hd)) :: acc)
        }
      }
      aux(ruby.split("/").toList, Nil)
    }

    def toHTML = <ruby>
      <rb>{cjk.toHtml}</rb>
      {multiRT(removeMarkup(ruby))}
    </ruby>
  }

  case class ReadingStart(src: String) extends Result
  case class Reading(src: String, content: TextResult) extends Result {
    def toWiki: String =
      s"""
         |'''$src''' ${content.toString }
       """.stripMargin

    def toHTML =
      <div>
        <b>{src}</b> {content.toHtml}
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
