package koktai

import scala.util.parsing.json.JSON
import Main._

/**
  * Created by pierre on 3/27/17.
  */
object PrepareMappings {

  def loadJson(path:String): Map[String, String] = {
    val src = scala.io.Source.fromFile(path)
    val data = JSON.parseFull(src.getLines() mkString "\n")
      .get
      .asInstanceOf[Map[String, String]]
    src.close()
    data
  }

  def convertHexaKeys(m: Map[String, String]):Map[Int, String] = {
    m.map { case (k, v) =>
      Integer.valueOf("f" + k, 16).toInt -> v
    }
  }

  def convertStringKeys(m: Map[String, String]):Map[Int, String] = {
    m.map { case (k, v) =>
      val ca = k.toCharArray
      require(Character.codePointCount(ca,0,ca.length) == 1)
      Character.codePointAt(ca,0) -> v
    }
  }



  def readAll():Map[FontFamily, Map[Int, String]] = {
    val m3 = loadJson("./Data/m3.json")
    val k =  loadJson("./Data/k.json")
    val nonAstral = loadJson("./Data/mapping.json")
    val missings = loadJson("./Data/missings.json")

    Map(
      FM3 -> convertHexaKeys(m3),
      FK  -> convertHexaKeys(k),
      NonAstral -> convertStringKeys(nonAstral),
      Unknown -> convertHexaKeys(missings)
    )
  }


}
