package koktai

import java.io.FileWriter

import scala.io.Source

/**
  * Created by pierre on 1/30/17.
  */
object Test  {

  /**
    * Read koktai source file and Group lines by sinogram
    *
    * @return a Sequence of multiline Strings, one for each sinogram entry in the dictionary
    */
  def readSino(): Seq[String] = {
    val src = Source.fromFile("/home/pierre/SRCs/koktai/all_recoded.txt")
    src.getLines().foldLeft(List(Nil):List[List[String]])(
      (acc, line) => line match {
        case "" => acc // ignore
        case ".本文" => acc //ignore
        case _ =>
          if (line.startsWith("<CHAR/>")) List(line)::acc //new sinogram
          else (line::acc.head)::acc.tail //append line
      }
    ).map(_.reverse.mkString("")) // rebuild one string per sinogram
  }

  def readChapters(): Seq[String] = {
    val src = Source.fromFile("/home/pierre/SRCs/koktai/all_recoded.txt")
    src.getLines().foldLeft(List(Nil):List[List[String]])(
      (acc, line) => line match {
        case "" => acc //ignore blank lines
        case ".本文" => acc //ignore
        case ".章首" => List(line.trim)::acc // new chapter
        case _ => (line.trim::acc.head)::acc.tail
      }
    ).reverseMap(_.reverse.mkString("")) // rebuild one string per chapter
  }
/*
  val data = read()

  val s = data.filter(s => s != "" && s != ".本文").reverse.mkString("").replace("~fm7bt0;","<EOL>").replace("~fm7;","<EOL>").replace("</rt><rt>","") + "<EOE>"))
    val r = s.map(KoktaiParser.parseSinogram)
    r.collect({case KoktaiParser.Failure(msg, next) =>
      s"$msg\n ${next.source} \n ${next.source.subSequence(0, next.offset)}"
    }).map(println)
    val failed = r.collect({case KoktaiParser.Failure(msg, next) => msg}).size
    val three = r.collect({case KoktaiParser.Success(x:KoktaiParser.SinogramEntry,y) => x}).map(_.taigiReadings.size).count(_ == 3)
    val two = r.collect({case KoktaiParser.Success(x:KoktaiParser.SinogramEntry,y) => x}).map(_.taigiReadings.size).count(_ == 2)
    val one = r.collect({case KoktaiParser.Success(x:KoktaiParser.SinogramEntry,y) => x}).map(_.taigiReadings.size).count(_ == 1)
    val zero = r.collect({case KoktaiParser.Success(x:KoktaiParser.SinogramEntry,y) => x}).map(_.taigiReadings.size).count(_ == 0)
    println(s"$failed failed, $three + $two + $one + $zero")
    r
  }*/


  def write() = {
    val data = readChapters()
    val dataSino = readSino()
    val reChar = "<CHAR".r

    def oldMethod(chptS: String): List[Sinogram] = {
      (for( oneChar <-reChar.split(chptS)) yield {
        KokTaiParser.parse(KokTaiParser.sinogram, "<CHAR" + oneChar)
      })
        .collect { case KokTaiParser.Success(x,y) => x}
        .toList
    }

    val finput = new FileWriter("/tmp/input")
    finput.write(data mkString "\n")
    finput.close()

    val finputSino = new FileWriter("/tmp/inputSino")
    finputSino.write(dataSino mkString "\n")
    finputSino.close()

    val chpts = (for (chptString <- data) yield {
      KokTaiParser.parse(KokTaiParser.chapter, chptString)
    }) collect {case KokTaiParser.Success(x,y) => Some(x) case _ => None}

    val charCounts = for (chptS <- data) yield { reChar.findAllMatchIn(chptS).size }
    val oldCounts = for (chptS <- data) yield { oldMethod(chptS).size}

    for (((parse, count),oc) <- chpts.zip(charCounts).zip(oldCounts)) {
      parse match {
        case None => println(s"chapter failed $count ($oc)")
        case Some(c) =>
          if (c.sinograms.size != count)
            println(s"chapter ${c.zhuyin} ${c.sinograms.size} / $count ($oc)")
          //else
            //println (s"${c.zhuyin} $count ok ($oc) ")


      }
    }
    chpts.flatten
  }

  def debug(n: Int) = {
    val data = readChapters().drop(n).head
    KokTaiParser.parse(KokTaiParser.chapter, data)

  }

  def findErrors() = {
    val data = readChapters()
    (for (chptString <- data) yield {
      (chptString, KokTaiParser.parse(KokTaiParser.chapter, chptString))
    }) collect {case (str,KokTaiParser.NoSuccess(error,input)) => (error, str)}
  }



}
