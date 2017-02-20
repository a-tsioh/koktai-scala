package koktai

import koktai.OldKoktaiParser.WordStart

import scala.util.parsing.combinator.RegexParsers
/**
  * Created by pierre on 1/30/17.
  */
object KokTaiParser extends RegexParsers {



  override def skipWhitespace = true
  override val whiteSpace = """([\s　 \n]|\.本文|~fm7t168(bb1)?;|~fm3t168(bb1)?;|~fm3bt180;|~bt0;|~fkt168bb1;|~fm3t84bb1;|~fm3;|~t84;|~fd6;|~fd0;|~bt180;|~t112fd0;|~t112;)+""".r

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
  }

  abstract sealed class TextResult extends Result
  type StringResult = String with Result
  type Zhuyin = StringResult
  type CJK = StringResult
  type Ruby = StringResult
  type Raw = StringResult

  case class Text(content: List[TextResult]) extends TextResult
  case class SomeChar(c:String) extends TextResult

  case class CJKRuby(cjk: String, ruby: String) extends TextResult {
    def toWiki: String = s"{{Ruby|$cjk|${ruby.replace("/","<br/")}}}"
  }

  case class ReadingStart(src: String) extends Result
  case class Reading(src: String, content: String) extends Result {
    def toWiki: String =
      s"""
         |'''$src''' ${cleanForWiki(content) }
       """.stripMargin
  }

  case class Word(title: String, num: Option[Int], text: Text) extends Result {

    def titleToWiki: String = {
      {re_markup.replaceAllIn(_:String,"")}
        .andThen
      {re_annot.replaceAllIn(_,"{{Ruby|$1|$2}}")}
        .apply(title)
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



  def chapter: Parser[Chapter] = ".章首" ~>
      rep(zhuyin | "/" | simpleAnnotReading) ~ opt("""\[[^\]]+\]""".r) ~
      opt(""".*?(?=<CHAR|~t96|$)""".r) ~
      rep(word) ~
      rep(sinogram) ^^ {
    case (zy ~ py ~ comment ~ words ~ sino ) => Chapter(zy mkString "", py.getOrElse("ai3???"), comment.getOrElse(""), sino, words)
  }

  def sinogram: Parser[Sinogram] = "<CHAR/>"  ~>
    //repsep(CjkSeq | Cjk | zhuyin, "/") ~ opt("/") ~
    sinogramTitle ~ opt("/") ~
    opt(simpleAnnotReading) ~
    """.*?(?=~fm7;[國台普]|~t96|<CHAR|$)""".r ~
    rep(reading) ~
    rep(word) ^^ {
    case ( title ~ _ ~ zhuyin ~ comment  ~ readings ~ words) =>
      Sinogram(title, zhuyin.getOrElse("".asInstanceOf[Ruby]), comment, readings, words)
  }
    //.getOrElse("".asInstanceOf[Ruby])



  def readingStart: Parser[ReadingStart ] = """~fm7(bt0)?;""".r ~> """國音|台甘|普 *閩""".r ^^ {ReadingStart(_)}
  def reading: Parser[Reading] = readingStart ~ """.+?(?=~fm7|<CHAR|~t96|$)""".r ^^ {case (start ~ str) => Reading(start.src,str)}

  def wordStart: Parser[WordStart.type] = "~t96;" ^^^ WordStart
  def word: Parser[Word] = wordStart ~>
    "【"  ~> """[^】]+""".r ~ "】" ~
    opt("\\d+".r) ~ wordContent ^^  {
    case (title ~ _ ~ num ~ content) => Word(title, num.map(_.toInt), content)}

  def wordContentAnyChar: Parser[TextResult] =
    annotedCJK   |
  simpleAnnotReading ^^ {SomeChar(_)}|
      "~(?!t96)".r ^^^ SomeChar("~") |
      "<(?!CHAR)".r ^^^ SomeChar("<") |
      "." ^^^ SomeChar(".") |
      "[^~<]".r ^^ {SomeChar(_)}

  def wordContent: Parser[Text] = rep( wordContentAnyChar ) <~ """(?=~t96|<CHAR|$)""".r ^^ {Text(_)}

  def zhuyin: Parser[Zhuyin] = """([主个·\u3105-\u312d\u31a0-\u31ba一ˊˇˋ˪˫ ͘ ]〾?)+""".r ^^ { _.asInstanceOf[Zhuyin]}
  //todo: c'est quoi ce 主 et ce 个

  def cjk: Parser[CJK] =
    """
      |([○
      |\u2F00-\u2FDF
      |\u2e80-\u2ef3
      |\u4e00-\u9fff
      |\u3400-\u4dff
      |\uF900-\uFAFF
      |\x{20000}-\x{2A6DF}
      |\x{2A700}-\x{2B73F}
      |\x{2B740}-\x{2B81F}
      |\x{2B820}-\x{2CEAF}
      |\x{2F800}-\x{2FA1F}]|<mark>&#......;</mark>)""".stripMargin.replace("\n","").r ^^ { _.asInstanceOf[CJK] }


  def sinogramTitle: Parser[Raw] =
    rep(cjk |"""~fk;|·|~fm7;|[…\( \)∟←→／/]""".r | zhuyin ) ^^ {_.mkString("").asInstanceOf[Raw]}

  def simpleAnnotReading: Parser[Ruby] =
    "<mark>&#xf856f;</mark>" ^^^ "│ㄋ".asInstanceOf[Ruby] |
      "<mark>&#xf815e;</mark>" ^^^ "│ㄋ".asInstanceOf[Ruby] |
      opt("·") ~ "<rt>" ~ repsep(zhuyin, "/") <~ "</rt>" ^^ {
        case ( point ~ _ ~  syllable) => s"${point.getOrElse("")} ${syllable.mkString("/")}".asInstanceOf[Ruby]
      }

  def annotedCJK: Parser[CJKRuby] = cjk ~ simpleAnnotReading ^^ {case (cjk ~ ruby ) => CJKRuby(cjk, ruby)}



  //def readingType: Parser[Raw] =
  //  ("(文白)" | "(文)" | "(語)" | "(白)"  | "(文語)" | "(常)" | "(另音)" | "(常音)" | "(舊音)" | "(古文)" | "(又音)" | "(字音)" | "(又讀）" ) ^^ {_.asInstanceOf[Raw]} // todo: continue ?





}
