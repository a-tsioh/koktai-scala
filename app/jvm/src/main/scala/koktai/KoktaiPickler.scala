package koktai

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

import boopickle.Default._

/**
  * Created by pierre on 5/29/17.
  */
class KoktaiPickler[T](pickle: (T => ByteBuffer), unpickle: (ByteBuffer => T)) {

  def ofFile(path: String): T = {
    val byteArray = Files.readAllBytes(Paths.get(path))
    unpickle(ByteBuffer.wrap(byteArray))
  }

  def toFile(path: String, thing: T) = {
    val fch = new FileOutputStream(new File(path)).getChannel
    fch.write(pickle(thing))
    fch.close()
  }
}

object sinogramPickler extends KoktaiPickler[Sinogram](Pickle.intoBytes(_), Unpickle[Sinogram].fromBytes(_) )
object chapterPickler extends KoktaiPickler[Chapter](Pickle.intoBytes(_), Unpickle[Chapter].fromBytes(_) )
