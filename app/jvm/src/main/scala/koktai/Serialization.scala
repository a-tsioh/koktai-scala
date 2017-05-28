package koktai

/**
  * Created by pierre on 5/28/17.
  */
object Serialization {
  case class Sinogram(cjk: TextResult, annot:String, comment: Option[String], readings: List[Reading],  words: List[Word])

}
