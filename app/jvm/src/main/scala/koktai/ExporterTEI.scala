package koktai

import java.io.{File, FileWriter}
import java.lang.Character.UnicodeScript

import koktai.KokTaiParser.parse
import koktai.Main.{convertAstralChars, loadIDSMapping, readByChapters}

import scala.xml.dtd.DocType
import scala.xml.{Elem, PCData}

object ExporterTEI {

  val mappingFile = "./Data/koktai-ids.csv"
  val contentFile = "./Data/recoded.u8"
  val missing = collection.mutable.HashSet.empty[String]

  /**
    * Abstract base class for new chars remapped to unicode or IDS
    */
  sealed abstract class NewChar {
    def metaData(): Elem
  }

  case class Hanji(font: String, ids: String, code: String) extends NewChar {
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"hj-$font-$code"}>
          <charName>TAIWANESE HANJI {ids}</charName>
          <mapping type="PUA">{s"U+$code"}</mapping>
          <mapping type="IDS">{ids}</mapping>
          <!--charProp>
          <unicodeName>general-category</unicodeName>
          <value>Lo</value>
        </charProp-->
          <!--desc>{ids}</desc-->
        </char>
      </charDecl>
  }

  case class HanjiNoIDS(font: String, code: String) extends NewChar {
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"missing-$font-$code"}>
          <charName>TAIWANESE HANJI {code} (IDS MISSING)</charName>
          <!--charProp>
          <unicodeName>general-category</unicodeName>
          <value>Lo</value>
        </charProp-->
        </char>
      </charDecl>
  }

  case class ToBeChecked(font: String, chr: String, code: Int) extends NewChar {
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"to-check-$font-${code.toHexString}"}>
          <charName>MAPPED CHAR TO BE CHECKED {code.toHexString} mapped to UNICODE {chr}</charName>
          <mapping type="standard">{chr}</mapping>
          <figure>
            <graphic url={s"img/k/${code.toHexString.drop(1)}.png"}/>
          </figure>
        </char>
      </charDecl>
  }
  case class MappedHanji(font: String, code: Int, cjk: String) extends  NewChar {
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"mapped-$font-${code.toHexString}"}>
          <charName>TAIWANESE HANJI {code.toHexString} mapped to UNICODE {cjk}</charName>
          <!--charProp>
          <unicodeName>general-category</unicodeName>
          <value>Lo</value>
        </charProp-->
          <mapping type="standard">{cjk}</mapping>
          <figure>
            <graphic url={s"img/k/${code.toHexString.drop(1)}.png"}/>
          </figure>
        </char>
      </charDecl>
  }

  case class TsuIm(font: String, id: String, code: String) extends NewChar {
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"ruby-$font-$code"}>
          <charName>TAIWANESE TSU-IM SYLLABLE {id}</charName>
          <mapping type="standard" style="writing-mode: vertical-rl">{id}</mapping>
          <mapping type="PUA">{s"U+$code"}</mapping>
        </char>
      </charDecl>
  }


  val newChars = collection.mutable.HashSet.empty[NewChar]

  Main.mappings = PrepareMappings.readAll()
  private val mapping = loadIDSMapping(mappingFile)


  def trToStr(tr: TextResult): Seq[xml.Node] = tr match {
    case koktai.Text(l) => l.flatMap(trToStr)
    case CJKRuby(cjk, ruby) =>
      val s = ruby.r.trim
      val c = ruby.code.toHexString
      newChars.add(TsuIm(ruby.font, s, ruby.code.toHexString))
      trToStr(cjk) ++ Seq(<g ref={s"ruby-${ruby.font}-$c"}>{s}</g>)
    case Ruby(r, f, c) =>
      val s = r.trim
      val hex = c.toHexString
      newChars.add(TsuIm(f, s, hex))
      <g ref={s"ruby-$f-$hex"}>{s}</g>
    case KokTaiCJK(cjk, f, code, true) =>
      newChars.add(MappedHanji(f, code, cjk))
      <g ref={s"mapped-$f-${code.toHexString}"}>{cjk}</g>
    case KokTaiToCheck(chr, f, code) =>
      newChars.add(ToBeChecked(f, chr,code))
      <g ref={s"to-check-$f-${code.toHexString}"}>{chr}</g>
    case KokTaiCJK(cjk, f, code, false) =>
      val hexa =  Integer.toHexString(Character.codePointAt(cjk,0))
      mapping.get(hexa.substring(1)) match {
        case Some(ids) =>
          newChars.add(Hanji(f, ids, hexa))
          <g ref={s"hj-$f-$hexa"}>{ids}</g>
        case None =>
          newChars.add(HanjiNoIDS(f,hexa))
          missing.add(hexa)
          <g ref={s"missing-$f-$hexa"}>\ufffd</g>
      }
    case sr:StringResult => Seq(xml.Text(sr.s))
  }

  def teiHeader(newChars: Seq[Elem]) =
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>國臺臺語辭典</title>
          <author>吳守禮</author>
          <respStmt>
            <resp>converted by</resp>
            <name>Pierre Magistry</name>
          </respStmt>
        </titleStmt>
        <publicationStmt>
          <publisher>TODO</publisher>
        </publicationStmt>
        <sourceDesc>
          <p>TODO</p>
        </sourceDesc>
      </fileDesc>
      <encodingDesc>
        <projectDesc>
          <p>todo</p>
        </projectDesc>
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
    val tei = templateTEI(newChars.toList.map(_.metaData()), teiContent)
    println("missing mappings")
    println(missing.toList.sorted.mkString("\n"))
    println(missing.size)
    tei
  }

  def buildXML(path: String): Unit = writeXML(buildDict(), path)

  def writeXML(doc: Elem, path: String): Unit = {
    import scala.xml.dtd.{DocType,SystemID}
    val fw = new FileWriter(path)
    scala.xml.XML.write(fw,doc,"utf-8",xmlDecl = true,DocType("TEI",SystemID("/tmp/tei_koktai.dtd"),Nil))
    fw.close()
  }


  def validateXML(xmlPath: String, schemaPath: String): Unit = {
    import javax.xml.transform.stream.StreamSource
    import javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI
    import javax.xml.validation._
    val factory = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI)
    val schema = factory.newSchema(new StreamSource(schemaPath))
    val validator = schema.newValidator()
    util.Try(validator.validate(new StreamSource(xmlPath))) match {
      case util.Failure(e) =>
        println(e.getMessage)

      case _ => println("ok")
    }
  }

}
