package koktai

import java.io.{File, FileWriter}
import java.lang.Character.UnicodeScript

import koktai.KokTaiParser.parse
import koktai.PrepareMappings.convertAstralChars

import scala.io.Source
import scala.xml.dtd.DocType
import scala.xml.{Elem, PCData, SAXException, SAXParseException}

object ExporterTEI {

  val mappingFile = getClass().getResource("/Data/koktai-ids.csv").getPath
  val contentFile = getClass().getResource("/Data/recoded.u8").getPath
  val missing = collection.mutable.HashSet.empty[String]

  /**
    * Abstract base class for new chars remapped to unicode or IDS
    */
  sealed abstract class NewChar {
    def metaData(): Elem
  }

  case class Hanji(font: String, ids: String, code: String) extends NewChar {
    val fontDir = font.drop(1).toLowerCase
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
          <figure>
            <graphic url={s"img/$fontDir/${code.drop(1)}.png"}/>
          </figure>
        </char>
      </charDecl>
  }

  case class HanjiNoIDS(font: String, code: String) extends NewChar {
    val fontDir = font.drop(1).toLowerCase
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"missing-$font-$code"}>
          <charName>TAIWANESE HANJI {code} (IDS MISSING)</charName>
          <!--charProp>
          <unicodeName>general-category</unicodeName>
          <value>Lo</value>
        </charProp-->
          <figure>
            <graphic url={s"img/$fontDir/${code.drop(1)}.png"}/>
          </figure>
        </char>
      </charDecl>
  }

  case class ToBeChecked(font: String, chr: String, code: Int) extends NewChar {
    val fontDir = font.drop(1).toLowerCase
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"to-check-$font-${code.toHexString}"}>
          <charName>MAPPED CHAR TO BE CHECKED {code.toHexString} mapped to UNICODE {chr}</charName>
          <mapping type="standard">{chr}</mapping>
          <figure>
            <graphic url={s"img/$fontDir/${code.toHexString.drop(1)}.png"}/>
          </figure>
        </char>
      </charDecl>
  }
  case class MappedHanji(font: String, code: Int, cjk: String) extends  NewChar {
    val fontDir = font.drop(1).toLowerCase
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
            <graphic url={s"img/$fontDir/${code.toHexString.drop(1)}.png"}/>
          </figure>
        </char>
      </charDecl>
  }

  case class TsuIm(font: String, id: String, code: String) extends NewChar {
    val fontDir = font.drop(1).toLowerCase
    override def metaData(): Elem =
      <charDecl>
        <char xml:id={s"ruby-$font-$code"}>
          <charName>TAIWANESE TSU-IM SYLLABLE {id}</charName>
          <mapping type="standard" style="writing-mode: vertical-rl">{id}</mapping>
          <mapping type="PUA">{s"U+$code"}</mapping>
          <figure>
            <graphic url={s"img/$fontDir/${code.drop(1)}.png"}/>
          </figure>
        </char>
      </charDecl>
  }

  def readByChapters(file: String): Seq[String] = {
    val reChar = "^~(fm7|fk|fm3)t168".r
    val src = Source.fromFile(file)
    src.getLines().foldLeft(List(Nil):List[List[String]])(
      (acc, line) => line match {
        case "" => acc //ignore blank lines
        case ".本文" => acc //ignore
        case ".章首" => List(line.trim)::acc // new chapter
        case _ =>
          val l = reChar.findFirstIn(line).map(_ => "<CHAR/>" + line).getOrElse(line)
          (l.trim::acc.head)::acc.tail
      }
    ).reverseMap(_.reverse.mkString("")) // rebuild one string per chapter
  }


  val newChars = collection.mutable.HashSet.empty[NewChar]

  //Main.mappings = PrepareMappings.readAll()
  private val mapping = PrepareMappings.loadIDSMapping(mappingFile)


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



  def chapterToTEI(chapter: Chapter, n: Int): Elem =
    <div n={n.toString}>
      <head>{chapter.zhuyin} [{chapter.pinyin}]</head>
      {chapter.comment.map(c => {
      if(n==1342) {println("ICI:"); println(c)}
      <p>{trToStr(c)}</p>
    }).orNull}{chapter.words match {
      case Nil => null
      case ws =>
        <div type="words">
          {ws.map(wordToTEI)}
        </div>
    }}{chapter.sinograms match {
      case Nil => null
      case sinos =>
        <div type="sinograms">
          {sinos.map(sinogramToTEI)}
        </div>
    }}
    </div>

  def wordToTEI(word: Word): Elem =
    <entry type="word">
      <form>
        <orth type="full">【{trToStr(word.title)}】</orth>
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
    </entry>

    def sinogramToTEI(sinogram: Sinogram): Elem =
      <entryFree type="sinogram">
        <form>
          <orth>{trToStr(sinogram.cjk)}</orth>
          <pron>{trToStr(sinogram.annot)}</pron>
          {sinogram.comment.map {fq => <note type="comment">{trToStr(fq)}</note> } orNull}
          {sinogram.readings.map {r => <note type={r.src}>{trToStr(r.content)}</note>}}
        </form>{if(sinogram.words.nonEmpty)
        <superEntry>
        {sinogram.words map wordToTEI}
        </superEntry>}
      </entryFree>
  //        {
  //        if (sinogram.words.isEmpty) null
  //        else {
  //          <list type="words">
  //            {sinogram.words map wordToTEI map { e => <item><div>{e}</div></item> }}
  //          </list>
  //        }
  //        }

    def parseData(): Seq[koktai.Chapter] = {
      val data = readByChapters(contentFile)
      val fw = new FileWriter(new File("/tmp/debug"))
      val txt = data
        .map {l => fw.write(l + "\n"); l}
        .map(convertAstralChars)
        .map {l => fw.write(l + "\n"); l}
        .seq
        .toList
      fw.close()
      val chapters = txt
          .par
        .map(parse(KokTaiParser.chapter, _))
        .collect { case KokTaiParser.Success(chpt,_) => chpt}
        .seq
      println(chapters.size, "chapters")
      chapters
    }

    def buildDict(): Elem = {
      val data = parseData()
      val teiContent = data.zipWithIndex.map {case (chpt,i) => chapterToTEI(chpt,i)}
      val tei = templateTEI(newChars.toList.map(_.metaData()), teiContent)
      println("missing mappings")
      println(missing.toList.sorted.mkString("\n"))
      println(missing.size)
      tei
    }

    def buildXML(path: String): Unit = writeXML(buildDict(), path)

    def writeXML(doc: Elem, path: String): Unit = {
      import scala.xml.dtd.{DocType,SystemID,PublicID}
      val fw = new FileWriter(path)
      scala.xml.XML.write(fw,doc,"utf-8",xmlDecl = true,DocType("TEI",PublicID("-//W3C//DTD XHTML 1.0 Strict//EN","http://koktai-beta.magistry.fr/static/tei_koktai.dtd"),Nil))
      fw.close()
    }

    def validateXML(xmlPath: String, schemaPath: String): Unit = {
      import javax.xml.transform.stream.StreamSource
      import javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI
      import javax.xml.validation._
      val factory = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI)
      val schema = factory.newSchema(new StreamSource(schemaPath))
      val validator = schema.newValidator()
      val xmlStream = new StreamSource(xmlPath)
      util.Try(validator.validate(xmlStream)) match {
        case util.Failure(e: SAXParseException) =>
          println(e.getMessage)
          println("line "+ e.getLineNumber)
        case util.Failure(e) => println(e.getMessage)
        case _ => println("ok")
      }
    }


  def writeToTmp(): Unit = {
    buildXML("/tmp/koktai-tei.xml")
  }

  def main(args: Array[String]): Unit = {
    writeToTmp()
  }



}
