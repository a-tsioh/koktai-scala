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

  def loadDodo(path: String): Map[Int, String] = {
    val src = scala.io.Source.fromFile(path)
    val data = src.getLines()
      .filterNot(_.startsWith("Big5"))
      .map(_.split(","))
      .collect {
        case Array(code, sinogram, user) =>
          val c = code.replace("+", "")
          if (sinogram != "Ⓧ" && c.length < 7) Some(Integer.valueOf(c.toLowerCase,16).toInt -> sinogram.trim)
          else None
      }
      .flatten
      .toMap
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
    val m3 = loadJson(getClass.getResource("/Data/m3.json").getPath) // tsu-im
    val k =  loadJson(getClass.getResource("/Data/k.json").getPath)  // tsu-im from k font
    val nonAstral = loadJson(getClass.getResource("/Data/mapping.json").getPath)
    val nonAstral2 = loadDodo(getClass.getResource("/Data/koktai-dodo-all.csv").getPath)  // mapped from k font
    val missings = loadJson(getClass.getResource("/Data/missings.json").getPath)  // still missing after crowdsourcing

    Map(
      FM3 -> convertHexaKeys(m3),
      FK  -> ((convertHexaKeys(k) ++ convertStringKeys(nonAstral)) ++ nonAstral2),
      NonAstral -> (nonAstral2 ++ convertStringKeys(nonAstral)),
      Unknown -> convertHexaKeys(missings)
    )
  }


}
