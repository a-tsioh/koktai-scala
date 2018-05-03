package koktai

import java.io.{File, FileWriter}
import java.lang.Character.UnicodeScript

import koktai.KokTaiParser.parse
import koktai.Main.{convertAstralChars, loadIDSMapping, readByChapters}

import scala.xml.{Elem, PCData}

object ExporterTEI {

  val mappingFile = "./Data/koktai-ids.csv"
  val contentFile = "./Data/recoded.u8"
  val missing = collection.mutable.HashSet.empty[String]

  sealed abstract class NewChar
  case class Hanji(ids: String, code: String) extends NewChar
  case class HanjiNoIDS(code: String) extends NewChar
  case class MappedHanji(code: Int, cjk: String) extends  NewChar
  case class TsuIm(id: String, code: String) extends NewChar
  val newChars = collection.mutable.HashSet.empty[NewChar]

  Main.mappings = PrepareMappings.readAll()
  private val mapping = loadIDSMapping(mappingFile)


  def trToStr(tr: TextResult): Seq[xml.Node] = tr match {
    case koktai.Text(l) => l.flatMap(trToStr)
    case CJKRuby(cjk, ruby) =>
      val s = ruby.r.trim
      val c = ruby.code.toHexString
      newChars.add(TsuIm(s, ruby.code.toHexString))
      trToStr(cjk) ++ Seq(<g ref={s"#$c"}>{s}</g>)
    case Ruby(r, c) =>
      val s = r.trim
      val hex = c.toHexString
      newChars.add(TsuIm(s, hex))
      <g ref={s"#$s"}>{s}</g>
    case KokTaiCJK(cjk, code, true) =>
      newChars.add(MappedHanji(code, cjk))
      <g ref={"#" + code.toHexString}>{cjk}</g>
    case KokTaiCJK(cjk, code, false) =>
      val hexa =  Integer.toHexString(Character.codePointAt(cjk,0))
      mapping.get(hexa.substring(1)) match {
        case Some(ids) =>
          newChars.add(Hanji(ids, hexa))
          <g ref={s"#$hexa"}>{ids}</g>
        case None =>
          newChars.add(HanjiNoIDS(hexa))
          missing.add(hexa)
          <g ref={s"#$hexa"}>{xml.EntityRef(hexa)}</g>
      }
    case sr:StringResult => Seq(xml.Text(sr.s))
  }

  def teiHeader(newChars: Seq[Elem]) =
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>國臺臺語辭典</title>
        </titleStmt>
        <publicationStmt></publicationStmt>
        <respStmt>
          <resp>converted by</resp>
          <name>Pierre Magistry</name>
        </respStmt>
      </fileDesc>
      <encodingDesc>
        <projectDesc>todo</projectDesc>
        {newChars}
      </encodingDesc>
    </teiHeader>


  def templateTEI(newChars: Seq[Elem], content: Seq[Elem]): Elem =
    <TEI>
      {teiHeader(newChars)}
      <text>
        <front></front>
        <body>{content}</body>
        <back></back>
      </text>
    </TEI>



  def chapterToTEI(chapter: Chapter): Elem =
    <div>
      <head>{chapter.zhuyin} [{chapter.pinyin}]</head>
      {chapter.comment.map(c => <p>{trToStr(c)}</p>).orNull}
      {chapter.words.map(wordToTEI) map {n => <item>{n}</item>} match {
      case Nil => null
      case words => <list rend="block" type="words">{words}</list>}
      }
      {chapter.sinograms.map(sinogramToTEI) map {n => <item>{n}</item>} match {
      case Nil => null
      case sinograms => <list rend="block" type="sinograms">{sinograms}</list>}
      }
    </div>

  def wordToTEI(word: Word): Elem =
    <entryFree>
      <form>
        <orth type="full">{trToStr(word.title)}</orth>
        <orth type="no-zhuyin">{word.noZhuyin}</orth>
        <pron>{word.onlyZhuyin}</pron>
      </form>
      {
        word.partOfSpeech.map { pos =>
          <gramGrp>
            <pos>{pos}</pos>
          </gramGrp>
        }.orNull
      }
      <def>{word.num.orNull}{trToStr(word.text)}</def>
    </entryFree>

  def sinogramToTEI(sinogram: Sinogram): Elem =
    <superEntry>
      <head>{trToStr(sinogram.cjk)}{trToStr(sinogram.annot)}</head>
      <list rend="inline" type="readings">
        {sinogram.comment.map {fq => <item>{trToStr(fq)}</item> } orNull}
        {sinogram.readings.map {r => <label>{r.src}</label><item>{trToStr(r.content)}</item>}}
      </list>
      <list rend="block" type="words">
      {sinogram.words map wordToTEI map {n => <item>{n}</item>}}
      </list>
    </superEntry>

  def hanjiMetaData(ids: String, code:String): Elem =
    <charDesc>
      <char id={s"$code"}>
        <charName>TAIWANESE HANJI {ids}</charName>
        <mapping type="PUA">{s"U+$code"}</mapping>
        <mapping type="IDS">{ids}</mapping>
        <charProp>
          <unicodeName>general-category</unicodeName>
          <value>Lo</value>
        </charProp>
      </char>
      <desc>{ids}</desc>
    </charDesc>

  def tsuImMetaData(r: String, code: String): Elem =
    <charDesc>
      <char id={s"$r"}>
        <charName>TAIWANESE TSU-IM SYLLABLE {r}</charName>
        <mapping type="standard" style="writing-mode: vertical-rl">{r}</mapping>
        <mapping type="PUA">{s"U+$code"}</mapping>
      </char>
    </charDesc>

  def missingChar(code: String): Elem =
    <charDesc>
      <char id={s"$code"}>
        <charName>TAIWANESE HANJI {code} (IDS MISSING)</charName>
        <charProp>
          <unicodeName>general-category</unicodeName>
          <value>Lo</value>
        </charProp>
      </char>
    </charDesc>

  def mappedChar(code: Int, cjk: String): Elem =
    <charDesc>
      <char id={code.toHexString}>
        <charName>TAIWANESE HANJI {code.toHexString} mapped to UNICODE {cjk}</charName>
        <charProp>
          <unicodeName>general-category</unicodeName>
          <value>Lo</value>
        </charProp>
        <mapping type="standard">{cjk}</mapping>
        <figure>
          <graphic url={s"img/k/${code.toHexString.drop(1)}.png"}/>
        </figure>
      </char>
    </charDesc>

  def newCharDecl(nc: NewChar): Elem = nc match {
    case TsuIm(id, code) => tsuImMetaData(id, code)
    case Hanji(ids, code) => hanjiMetaData(ids, code)
    case HanjiNoIDS(code) => missingChar(code)
    case MappedHanji(code, cjk) => mappedChar(code, cjk)
  }

  def parseData(): Seq[koktai.Chapter] = {
    val data = readByChapters(contentFile)
    val txt = data.par
      .map(convertAstralChars)
    val fw = new FileWriter(new File("/tmp/debug"))
    txt.seq.foreach(l => fw.write(s"$l\n"))
    fw.close()
    txt
      .map(parse(KokTaiParser.chapter, _))
      .collect { case KokTaiParser.Success(chpt,_) => chpt}
      .seq
  }

  def buildDict(): Elem = {
    val data = parseData()
    val teiContent = data.map(chapterToTEI)
    val tei = templateTEI(newChars.toList.map(newCharDecl), teiContent)
    println(missing.toList.sorted.mkString("\n"))
    println(missing.size)
    tei
  }

  def buildXML(path: String): Unit = writeXML(buildDict(), path)

  def writeXML(doc: Elem, path: String): Unit = {
    val fw = new FileWriter(path)
    scala.xml.XML.write(fw,doc,"utf-8",true,null)
    fw.close()
  }

}
