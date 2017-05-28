package koktai

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by pierre on 11/8/16.
  */
object OldKoktaiParser extends RegexParsers {

  sealed abstract class Lang
  case object Mandarin extends Lang
  case object Taiwanese extends Lang

  sealed abstract class Kind
  abstract class AnyDef extends Kind
  case object Def extends AnyDef
  case object WenDef extends AnyDef
  case object YuDef extends  AnyDef
  case object Ex extends Kind
  case object Rel extends Kind


  def splitWenYu(s: Sentence): List[Sentence] ={
    val annot = s.annot
    def aux(toBeRead: List[Token], buf: List[Token]): List[Sentence] = toBeRead match {
      case Wen::tail => Sentence(annot, (Wen::buf).reverse)::aux(tail,Nil)
      case Yu::tail => Sentence(annot, (Yu::buf).reverse)::aux(tail,Nil)
      case Nil => List(Sentence(annot,buf.reverse))
      case hd::tail => aux(tail, hd::buf)
    }
    aux(s.tokens, Nil)
  }


  def confirmTaigi(s:Sentence): Boolean = {
    val total = math.max(s.tokens.map(_.size).foldLeft(0)(_ + _).toDouble,1)
    val annoted = s.tokens.map(_.annotationSize).foldLeft(0)(_ + _)

    annoted / total > 0.5
  }
  case class Category(lang:Lang, kind: Kind)
  def analyze(de: DictEntry): List[(Category, Sentence)] = {
    def aux(state: Category, toBeRead: Seq[Sentence], buf: List[(Category, Sentence)]): List[(Category, Sentence)] = {
      toBeRead match {
        case Nil => buf.reverse
        case head:: tail =>head.annot match {
          case None =>
            val lang = if(state.lang == Taiwanese && confirmTaigi(head)) Taiwanese else Mandarin
            val newState = Category(lang, state.kind)
            aux(newState, tail, (newState, head)::buf )
          case Some(AnnotSentence("例：")) => aux(Category(Mandarin, Ex), tail, (Category(Mandarin, Ex),head)::buf)
          case Some(AnnotSentence("(台)")) =>
            val newState = Category(Taiwanese, state.kind)
            aux(newState, tail, (newState, head)::buf)
          case Some(AnnotSentence("(國語)")) =>
            val newState = Category(Mandarin, state.kind)
            aux(newState, tail, (newState, head)::buf)
          case Some(AnnotSentence("台類語：")) =>
            val newState = Category(Taiwanese, Rel)
            aux(newState, tail, (newState, head)::buf)
        }

      }
    }
    aux(Category(Mandarin, Def), de.body, Nil)
  }

  case class ExtractedInfo(headWord:(String, String), translations:List[(String, String)], relSet:List[(String,String)])
  def extractInfo(dict: Seq[DictEntry]) = {
    for (de <- dict) yield {
      val headWord = de.entry
      val analyzed = analyze(de)
      val sentences = analyzed
        .collect({ case (Category(Taiwanese, _:AnyDef), s) => s})
        .flatMap(splitWenYu)
        .filter(s => s.size <= 8)
        .filter(s => s.tokens.forall(_.size <= 4))
      val words =
        sentences
          .flatMap(s => s.tokens.collect({case tok:CjkSeq => tok case tok:AnnotedToken => tok  }))
            .map(w => (extractWordForm(w),extractReading(w)))

      val rels =
        analyzed
        .collect({case (Category(Taiwanese, Rel), s) => s})
        .flatMap(s => s.tokens.collect({case tok:CjkSeq => tok case tok:AnnotedToken => tok  }))
        .map(w => (extractWordForm(w), extractReading(w)))

      ExtractedInfo((headWord.mkString(""),extractReading(CjkSeq(headWord))), words, rels)
    }
  }

  def extractReading(t:Token): String = {
    t match {
      case CjkSeq(toklist) => toklist.map(extractReading).mkString(" ")
      case AnnotedToken(_,annotation) => annotation match {
        case SimpleAnnotReading(r) => r.mkString(" ")
        case SoundChange(from, to) => from.toString
      }
      case _ => ""
    }
  }

  def extractWordForm(t:Token): String = {
    t match {
      case CjkSeq(toklist) => toklist.map(extractWordForm).mkString("")
      case Cjk(c) => c
      case Variant(t,_) => extractWordForm(t)
      case AnnotedToken(t,_) => extractWordForm(t)
      case _ => ""
    }
  }

  def getLeiYu(l: Seq[DictEntry]): Seq[DictEntry] = {
    l.filter(d => {
      d.body.exists({case Sentence(Some(AnnotSentence(a)),_) => a == "台類語：" case _ => false })
    })
  }

  //lexical.delimiters += ("<", "/", ">", "[", "]", "。")
  //lexical.reserved += ("null", "true", "false")
  override def skipWhitespace = true
  override val whiteSpace = """[\s　]+""".r

  abstract sealed class Token {
    def size = 1
    def annotationSize = 0
  }

  case class Zhuyin(zy: String) extends Token {
    override def toString = s"$zy"
  }
  def zhuyin: Parser[Zhuyin] = """([主个·\u3105-\u312d\u31a0-\u31ba一ˊˇˋ˪˫ ͘ ]〾?)+""".r ^^ { Zhuyin(_)}
  //todo: c'est quoi ce 主 et ce 个

  case class Cjk(c: String) extends Token {
    override def toString = c
  }
  def CJK: Parser[Token] =
    """
      |…?([○
      |\u2F00–\u2FDF
      |\u2e80-\u2ef3
      |\u4e00-\u9fff
      |\u3400-\u4dff
      |\uF900-\uFAFF
      |\x{20000}-\x{2A6DF}
      |\x{2A700}–\x{2B73F}
      |\x{2B740}–\x{2B81F}
      |\x{2B820}-\x{2CEAF}
      |\x{2F800}-\x{2FA1F}]|<mark>&#......;</mark>)·?""".stripMargin.replace("\n","").r ^^ { Cjk(_) }


  case object Tilde extends Token
  def tilde: Parser[Token] = "～" ^^^ Tilde


  abstract class Operators extends Token
  abstract class Arrow extends Operators

  def arrow: Parser[Arrow] = rightArrow | leftArrow

  case object  Plus extends Operators
  def plus:Parser[Operators] = "＋" ^^^ Plus

  case object RightArrow extends Arrow
  def rightArrow: Parser[Arrow] = "→" ^^^ RightArrow

  case object LeftArrow extends Arrow
  def leftArrow: Parser[Arrow] = "←" ^^^ LeftArrow


  case class Latin(l: String) extends Token
  def latin: Parser[Latin] = "[a-zA-Z]+".r ^^ { Latin(_) }

  case class Numeric(n: String) extends Token
  def numeric: Parser[Numeric] = "-?[0-9]+\\.?[0-9]*".r ^^ { Numeric(_) }

  case class Punct(p: String) extends Token
  def punct: Parser[Punct] = """[～…\[\]/：，！？、（）()──「」【】＝*；,｛｝{}]""".r ^^ { Punct(_) }
  // todo: checker le sens du :
  // ＝

  case class CircledNumber(n:String) extends Token
  def circledNumbers: Parser[CircledNumber] = "[\u2460-\u2473]".r ^^ { CircledNumber(_) }

  def zhuyinAlt: Parser[List[Zhuyin]] = repsep(zhuyin, "/")

  abstract class AnnotReading extends Token
  def annotReading: Parser[AnnotReading] = soundChange | simpleAnnotReading

  case class SimpleAnnotReading(r: List[Zhuyin]) extends AnnotReading {
    override def toString = r.mkString("/")
  }
  def simpleAnnotReading: Parser[AnnotReading] = "<rt>" ~> zhuyinAlt <~ "</rt>" ^^ { SimpleAnnotReading(_) }

  case class SoundChange(from: AnnotReading, to: AnnotReading) extends AnnotReading
  def soundChange: Parser[AnnotReading] = simpleAnnotReading ~ rightArrow ~ simpleAnnotReading ^^ {case from ~ _ ~  to  => SoundChange(from, to)}

  sealed case class AnnotSentence(a: String)
  def annotSentence: Parser[AnnotSentence] = ( "(台)" | "例：" | "(國語)" | "台類語：" ) ^^ { case s: String => AnnotSentence(s) }


  case class Sentence(annot: Option[AnnotSentence], tokens: List[Token]) {
    override def toString = s"${annot.map(_.toString).getOrElse("")} ${tokens.mkString(" ")}"
    def size = tokens.foldLeft(0)(_ + _.size)
  }
  def sentence: Parser[Sentence] =
    (opt(annotSentence) ~ token  ~ rep(token) <~ "。") ^^ { case (annot ~ hd ~ tl ) => Sentence(annot, hd::tl)}

  def body: Parser[List[Sentence]] = sentence ~ rep(sentence) ^^ {case (hd ~ tl) => hd::tl}

  def token: Parser[Token] = mark | CJKSeq | rawToken

  case class AnnotedToken(t: Token, annotation: AnnotReading) extends Token {
    override def toString = t.toString
    override def size = 1
    override def annotationSize = 1
  }
  def annotedCJK: Parser[AnnotedToken] = CJK ~ annotReading  ^^
    {case (t ~ a) => AnnotedToken(t,a)}

  case class Variant(t: Token, alternatives: List[Token]) extends Token {
    override def size = 1
    override def annotationSize = 1
  }
  def graphVar: Parser[AnnotedToken] =
    ( CJK ~ "(/" ~ repsep(CJK,"/") ~ ")" ~ annotReading )  ^^
      {case (t ~ "(/" ~ a ~ ")" ~ r) =>  AnnotedToken(Variant(t, a),r) }  |
      ( CJK ~ "/" ~ repsep(CJK,"/") ~ annotReading ) ^^
        { case (t ~ "/" ~ a ~ r) => AnnotedToken(Variant(t, a), r)}


  abstract class Mark extends Token {
    override def size = 0
  }
  case object Wen extends Mark
  case object Yu extends Mark

  def mark: Parser[Mark] = wen | yu
  def wen: Parser[Mark] = "(文)" ^^^ Wen
  def yu: Parser[Mark] = "(語)" ^^^ Yu

  def lexVar: Parser[Variant] = annotedCJK ~ "(/" ~ (annotedCJK | CJK) ~ ")" ^^ {case (a ~ _ ~ v ~ _) => Variant(a,List(v))}

  case class CjkSeq(s: List[Token]) extends Token {
    override def toString = s"seq(${s.mkString("")})"
    override def size = s.map(_.size).foldLeft(0)(_ + _)
    override def annotationSize = s.map(_.annotationSize).foldLeft(0)(_ + _)
  }
  def CJKSeq: Parser[CjkSeq] =
    ( ( lexVar | graphVar | annotedCJK | CJK) ~ rep( lexVar | graphVar | annotedCJK | CJK | plus ) ) ^^ {case (hd ~ tl) => CjkSeq(hd::tl)}

  def rawToken: Parser[Token] = numeric | latin | tilde | arrow | punct | annotReading | circledNumbers

  def pos: Parser[CjkSeq] = "[" ~> CJKSeq <~ "]"


  case class EntryPunct(p: String) extends Token
  def entryPunct: Parser[EntryPunct] = """[…\[\]/：，！？、（）()──「」＝*]""".r ^^  { EntryPunct(_) }

  case class DictEntry(entry: List[Token], homonymN: Option[Int], partOfSpeech: Option[CjkSeq], body:List[Sentence]) {
    override def toString =
      s"""
         | ${entry.mkString("")}${homonymN.getOrElse(0)} [${partOfSpeech}]
         | ${body.mkString("\n")}
       """.stripMargin
  }
  def dictEntry: Parser[DictEntry] =
    "【" ~ rep(annotedCJK | graphVar | lexVar | entryPunct | arrow ) ~ "】" ~ opt("[0-9]+".r)  ~ opt(pos) ~ body ^^
      {case (_ ~ entry ~ _ ~ n ~  p  ~ content ) => DictEntry(entry, n.map(_.toInt), p, content)}


  def apply(s: String) = {
    val formatRE = "~[a-z0-9]+;".r
    val replacePointRE = "·<rt>".r
    parse(dictEntry, replacePointRE.replaceAllIn(formatRE.replaceAllIn(s,""),"<rt>·").replaceAll("</rt>/<rt>","/"))
  }

  def parseSinogram(s: String) = {
    val formatRE = "~[a-z0-9]+;".r
    val replacePointRE = "·<rt>".r
    parse(sinogram, replacePointRE.replaceAllIn(formatRE.replaceAllIn(s,""),"<rt>·").replaceAll("</rt>/<rt>","/"))
  }

  // parser bas niveau (remplace le python mais pas le recodage en perl)

  def fontChange: Parser[String] = "~fk[a-z0-9]*;.*?~fm3[a-z0-9]*;".r ^^ {x => "x" }


  // parse des lectures de sinogrammes

  case object EOL
  def eol: Parser[EOL.type] =
    "~fm7(bt0)?;".r ^^^
      EOL // End Of Line

  case object WordStart
  def wordStart: Parser[WordStart.type] =
    "~t96;" ^^^
      WordStart

  //case object EOE
  //def eoe: Parser[EOE.type] = "<EOE>" ^^^ EOE // End Of Entry

  abstract sealed class Sinogram
  case class SinogramEntry(form: List[Token], mdnReading: Option[AnnotReading], taigiReadings: List[Reading]) extends Sinogram
  case class SinogramLink(source: Token, target: Token) extends Sinogram

  def sinogram: Parser[Sinogram] = "<CHAR/>" ~ repsep(CJK | zhuyin,"/") ~
    opt(simpleAnnotReading) ~ opt("＝" ~ simpleAnnotReading) ~ rep(token | "。") ~
    opt(eol ~ repsep(reading,eol)) ~
    opt(wordStart ~ repsep(dictEntry, wordStart)) ^^{
    case (_ ~ sinograms ~ zhuyin ~ _ ~ remains ~ Some(_ ~ readings) ~ words) =>
      SinogramEntry(sinograms, zhuyin, readings.flatten)
    case (_ ~ sinograms ~ zhuyin ~ _ ~ remains ~ None ~ words) =>
      SinogramEntry(sinograms, zhuyin, Nil)
  } |
  "<CHAR/>" ~> CJK ~ "＝" ~ CJK ^^ {case (src ~ _ ~ tgt) => SinogramLink(src, tgt)}

  case class Reading(source: String, readings: List[ReadingInfo])

  def reading: Parser[Option[Reading]] =
    readingSource ~ readingSentence ~ rep(readingSentence) ~ opt("∥") ^^ {
    case ( source ~ sentencesHead ~ sentencesTail ~ _ ) =>
      Some(Reading(source, (sentencesHead::sentencesTail).flatten))
    } |
    readingSource ~ opt("。")  ~ opt("∥") ^^^ None |
    "如：" ~ rep(token | "。") ^^^ None |
    "互見：" ~rep(token | "。") ^^^ None |
    "類語：" ~rep(token | "。") ^^^ None |
    ("辨似：" <~ rep(token | "。")) ^^^ None |
  "「" ~> CJK <~ "」的"  ~  rep(token) ~  "。" ^^^ None  | // todo: get the link
  rep(token | "。") ^^^ None // anything else (fetch and anlyase


  def readingSource: Parser[String] = "國音" | "台甘" | "普閩" | ("台" ~ "甘") ^^^ "台甘"


  def readingSentence: Parser[Option[ReadingInfo]] =
    (readingInfo <~ rep( "[^<∥]".r | simpleAnnotReading) <~ opt("。") ) ^^ {case r => Some(r)} |
      ("按：" <~ rep(token | simpleAnnotReading) <~ opt("。")) ^^^ None |
      ("未收" <~ rep(token | zhuyin) <~ opt("。")) ^^^ None |
      ( (token | zhuyin) ~ rep(token | zhuyin) <~ opt("。")) ^^^ None


  def readingPreAnnot: Parser[String] = "(文白)" | "(文)" | "(語)" | "(白)"  | "(文語)" | "(常)" | "(另音)" | "(常音)" | "(舊音)" | "(古文)" | "(又音)" | "(字音)" | "(又讀）"
    (pos ^^ {_.toString}) // todo: continue

  def readingPostAnnot: Parser[String] = "(泉)" | "(漳)" | "(方音)" // todo: complete

  case class ReadingInfo(annot: List[String], reading: AnnotReading, postAnnots: List[String])
  def readingInfo: Parser[ReadingInfo] =
    repsep(readingPreAnnot, "，?".r) ~ simpleAnnotReading ~ rep(readingPostAnnot) ^^ {
      case (annot ~ reading ~ postAnnot) => ReadingInfo(annot, reading, postAnnot)}

}

