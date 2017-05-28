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
  class Sinogram(val cjk: TextResult, val ruby: SimpleString, val comment: TextResult)  extends js.Object
  implicit def SinogramFromParse(s: koktai.Sinogram): Sinogram = {
    new Sinogram(
      TextResultFromParse(s.cjk),
      SimpleStringFromParse(s.annot),
      s.comment
        .map(TextResultFromParse)
        .getOrElse(new SimpleString(""))
    )
  }


  @ScalaJSDefined
  abstract sealed class TextResult(val t: String) extends js.Object

  implicit def TextResultFromParse(s: koktai.TextResult): TextResult = s match {
    case s: StringResult => SimpleStringFromParse(s)
    case koktai.Text(content) => new Text(content.map(TextResultFromParse).toJSArray)
    case koktai.CJKRuby(cjk, ruby) => new CJKRuby(TextResultFromParse(cjk), ruby.r)
    case koktai.KokTaiCJK(cjk) => new KoktaiCJK(cjk)
  }

  // mirror StringResult
  @ScalaJSDefined
  class SimpleString(val s: String) extends TextResult("SimpleString")
  implicit def SimpleStringFromParse(s: StringResult): SimpleString = new SimpleString(s.s)


  @ScalaJSDefined
  class Text(val content: js.Array[TextResult]) extends TextResult("Text")

  @ScalaJSDefined
  class CJKRuby(text: TextResult, ruby: String) extends TextResult("CJKRuby")

  @ScalaJSDefined
  class KoktaiCJK(cjk: String) extends TextResult("KoktaiCJK")



}
