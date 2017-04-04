package koktai

import koktai.OldKoktaiParser.WordStart

import scala.util.parsing.combinator.RegexParsers
/**
  * Created by pierre on 1/30/17.
  */
object KokTaiParser extends RegexParsers {



  override def skipWhitespace = true
  override val whiteSpace = """([\s　 \n]|\.本文|~fm7t168bb1;|~fm7t168;|~fm3t168bb1;|~fm3t168;|~fm3bt180;|~bt0;|~fkt168bb1;|~fm3t84bb1;|~fk;|~fm3;|~t84;|~fd6;|~fd0;|~bt180;|~t112fd0;|~t112;|~fb7bb1;|~fm3bb1;|~bt180;|~bt0;|~bt315;|~fm3t42;)+""".r








  def chapter: Parser[Chapter] = ".章首" ~>
      rep(zhuyin | "/" | simpleAnnotReading) ~ opt("""\[[^\]]+\]""".r) ~
    opt(wordContent) ~
   // opt(""".*?(?=<CHAR|~t96|~fm7;[國台普]$)""".r) ~
      rep(word) ~
      rep(sinogram) ^^ {
    case (zy ~ py ~ comment ~ words ~ sino ) => Chapter(zy mkString "", py.getOrElse("ai3???"), comment, sino, words)
  }

  def sinogram: Parser[Sinogram] = "<CHAR/>"  ~>
    //repsep(CjkSeq | Cjk | zhuyin, "/") ~ opt("/") ~
    sinogramTitle ~ opt("/") ~
    opt(simpleAnnotReading) ~
    opt(wordContent) ~
    // """.*?(?=~fm7;[國台普]|~t96|<CHAR|$)""".r ~
    rep(reading) ~
    rep(word) ^^ {
    case ( title ~ _ ~ zhuyin ~ comment  ~ readings ~ words) =>
      // todo: correct point · not inside <rt>
      Sinogram(title, zhuyin.getOrElse("".asInstanceOf[Ruby]), comment, readings, words)
  }
    //.getOrElse("".asInstanceOf[Ruby])



  def readingStart: Parser[ReadingStart ] = """~fm7(bt0)?;""".r ~> """國音|台甘|普 *閩""".r ^^ ReadingStart
  def reading: Parser[Reading] = readingStart ~ wordContent ^^ {case (start ~ content) => Reading(start.src, content)}

  def wordStart: Parser[WordStart.type] = "~t96;" ^^^ WordStart
  def word: Parser[Word] = wordStart ~>
    "【"  ~> rep(wordTitleAnyChar) ~ "】" ~
    opt("\\d+".r) ~ wordContent ^^  {
    case (title ~ _ ~ num ~ content) => Word(Text(title), num.map(_.toInt), content)}

  def wordTitleAnyChar: Parser[TextResult] =
  annotedCJK |
    koktaiCjk |
    simpleAnnotReading ^^ SomeChar |
    "[^】]".r ^^ SomeChar

  def wordContentAnyChar: Parser[TextResult] =
    annotedCJK   |
      koktaiCjk |
  simpleAnnotReading ^^ SomeChar |
      "~(?!t96|fm7;[國台普])".r ^^^ SomeChar("~") |
      "<(?!CHAR)".r ^^^ SomeChar("<") |
      "." ^^^ SomeChar(".") |
      "[^~<]".r ^^ SomeChar

  def wordContent: Parser[Text] = rep( wordContentAnyChar ) <~ """(?=~t96|<CHAR|~fm7;[國台普]|$)""".r ^^ Text

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
      |\x{2F800}-\x{2FA1F}])""".stripMargin.replace("\n","").r ^^ CJK

  def koktaiCjk: Parser[KokTaiCJK] =
      "<mark>" ~> ".".r <~ "</mark>" ^^ {case cjk => KokTaiCJK(cjk)}


  def sinogramTitle: Parser[TextResult] =
    rep(sinogramTitleAnyElem) ^^ Text

  def sinogramTitleAnyElem: Parser[TextResult] =
    koktaiCjk |
      cjk |
      """~fk;|~fm7;|[…\( \)∟←→／/]""".r ^^ SomeChar |
      zhuyin ^^ SomeChar

  def simpleAnnotReading: Parser[Ruby] =
    "<mark>&#xf856f;</mark>" ^^^ "│ㄋ".asInstanceOf[Ruby] |
      "<mark>&#xf815e;</mark>" ^^^ "│ㄋ".asInstanceOf[Ruby] |
      opt("·") ~ "<rt>" ~ repsep(zhuyin, "/") <~ "</rt>" ^^ {
        case ( point ~ _ ~  syllable) => s"${point.getOrElse("")} ${syllable.mkString("/")}".asInstanceOf[Ruby]
      }

  def annotedCJK: Parser[CJKRuby] =
    koktaiCjk  ~ simpleAnnotReading ^^ {case (cjk ~ ruby ) => CJKRuby(cjk, ruby)} |
      cjk ~ simpleAnnotReading ^^ {case (cjk ~ ruby ) => CJKRuby(cjk, ruby)}



  //def readingType: Parser[Raw] =
  //  ("(文白)" | "(文)" | "(語)" | "(白)"  | "(文語)" | "(常)" | "(另音)" | "(常音)" | "(舊音)" | "(古文)" | "(又音)" | "(字音)" | "(又讀）" ) ^^ {_.asInstanceOf[Raw]} // todo: continue ?





}
