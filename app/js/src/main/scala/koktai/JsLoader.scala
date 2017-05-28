package koktai

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.ByteBuffer

import boopickle.Default._

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom

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

  def getSinogram: (String, js.Function1[js.Object, js.Any]) => Unit = getSomething({bb =>
    val s =Unpickle[Sinogram].fromBytes(bb)
    JsData.SinogramFromParse(s)
  })
  def getChapter: (String, js.Function1[js.Object, js.Any]) => Unit = getSomething(Unpickle[Chapter].fromBytes)
}
