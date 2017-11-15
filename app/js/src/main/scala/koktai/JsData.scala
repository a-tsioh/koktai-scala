package koktai

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined
import scala.scalajs.js.JSConverters._

/**
  * Created by pierre on 5/29/17.
  */
@ScalaJSDefined
object JsData  extends js.Object {


//  case class Sinogram(cjk: TextResult, annot:Ruby, comment: Option[TextResult], readings: List[Reading],  words: List[Word]) extends Result {
  @ScalaJSDefined
  class Sinogram(val cjk: TextResult, val ruby: String, val comment: TextResult, val readings: js.Array[Reading], val words: js.Array[Word])  extends js.Object
  implicit def SinogramFromParse(s: koktai.Sinogram): Sinogram = {
    new Sinogram(
      TextResultFromParse(s.cjk),
      s.annot.s,
      s.comment
        .map(TextResultFromParse)
        .getOrElse(new SimpleString("")),
      s.readings.map(ReadingFromParse).toJSArray,
      s.words.toArray.map(WordFromParse).toJSArray
    )
  }

  @ScalaJSDefined
  class Reading(val src: String, val content: TextResult) extends js.Object
  implicit def ReadingFromParse(r: koktai.Reading): Reading = {
    new Reading(r.src, TextResultFromParse(r.content))
  }


//  case class Chapter(zhuyin: String, pinyin: String, comment: Option[TextResult], sinograms: List[Sinogram], words: List[Word]) extends Result
  @ScalaJSDefined
  class Chapter(val index: Int, val zhuyin: String, val pinyin: String, val comment: TextResult, val sinograms: js.Array[Sinogram]) extends js.Object

  implicit def ChapterFromParse(i: Int, c: koktai.Chapter): Chapter = {
    new Chapter(
      i,
      c.zhuyin,
      c.pinyin,
      c.comment
        .map(TextResultFromParse)
        .getOrElse(new SimpleString("")),
      c.sinograms.map(SinogramFromParse).toJSArray
    )

  }

  //case class Word(title: Text, num: Option[Int], text: Text) extends Result {

  @ScalaJSDefined
  class Word(val title: TextResult, val num: Option[Int], val definition: TextResult) extends js.Object

  implicit def WordFromParse(w: koktai.Word): Word = {
    new Word(
      TextResultFromParse(w.title),
      w.num,
      TextResultFromParse(w.text)
    )
  }


  @ScalaJSDefined
  abstract sealed class TextResult(val t: String) extends js.Object

  implicit def TextResultFromParse(s: koktai.TextResult): TextResult = s match {
    case s: StringResult => SimpleStringFromParse(s)
    case koktai.Text(content) => new Text(content.map(TextResultFromParse).toJSArray)
    case koktai.CJKRuby(cjk, ruby) => new CJKRuby(TextResultFromParse(cjk), ruby.r)
    case koktai.KokTaiCJK(cjk) => println(cjk) ; new KoktaiCJK(cjk)
  }

  // mirror StringResult
  @ScalaJSDefined
  class SimpleString(val s: String) extends TextResult("SimpleString")
  implicit def SimpleStringFromParse(s: StringResult): SimpleString = new SimpleString(s.s)


  @ScalaJSDefined
  class Text(val content: js.Array[TextResult]) extends TextResult("Text")

  @ScalaJSDefined
  class CJKRuby(val text: TextResult,val ruby: String) extends TextResult("CJKRuby")

  @ScalaJSDefined
  class KoktaiCJK(val cjk: String) extends TextResult("KoktaiCJK")



}
