package koktai

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.ByteBuffer

import boopickle.Default._

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom
import scala.scalajs.js.JSConverters._

import scala.concurrent.duration.Duration
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}

/**
  * Created by pierre on 5/28/17.
  */
@ScalaJSDefined
@JSExportTopLevel("koktaiAPI")
object JsLoader extends js.Object {

  def getSomething[T](unpickle: ByteBuffer => T) = { (url: String, callback: js.Function1[js.Object,js.Any]) =>
    println(s"requesting $url")
    val fut = dom.ext.Ajax.get(url, responseType = "arraybuffer")
    .map { xhr =>
      val bb = TypedArrayBuffer.wrap(xhr.response.asInstanceOf[ArrayBuffer])
      unpickle(bb).asInstanceOf[js.Object]
    }
    .foreach(callback)
  }



  def getSinogram: js.Function2[String, js.Function1[js.Object, js.Any], Unit] = getSomething({bb =>
    val s =Unpickle[Sinogram].fromBytes(bb)
    JsData.SinogramFromParse(s)
  })


  def getChapter: js.Function2[String, js.Function1[js.Object, js.Any], Unit] = getSomething {bb =>
    val (i,ch) =Unpickle[(Int,Chapter)].fromBytes(bb)
    JsData.ChapterFromParse(i,ch)
  }

  def getIndex: js.Function2[String, js.Function1[js.Object, js.Any], Unit] = getSomething({ bb =>
    val m = Unpickle[Map[String, Map[String, Int]]].fromBytes(bb)

    new js.Object {
      def get(zhuyin: String) = m.get(zhuyin.substring(0,1)).map(_.get(zhuyin)).getOrElse("0")
      def getAll() =
        m.toList.sortBy(_._1).map( l =>
          Array(l._1,
          l._2.toList.sorted.map {case (syl,i) => Array(syl, i.toString).toJSArray}.toJSArray).toJSArray).toJSArray
    }
  })
}
