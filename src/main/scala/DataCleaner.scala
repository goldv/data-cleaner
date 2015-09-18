import java.io.{PrintWriter, File}

import scala.util.Try

/**
 * @author <a href="mailto:vgolding@algotrader.ch">Vince Golding</a>
 *
 * @version $Revision$ $Date$
 */
object DataCleaner extends App{


  val source = scala.io.Source.fromFile(args(0))

  val writer = new PrintWriter(new File(s"${args(0)}.clean"))

  source.getLines().filter(validateTick).map(l => s"${l}\n").foreach(writer.write)

  def validateTick(line: String) = {
    val arr = line.split(",")
    val result = for{
      dateTime <- Try(arr(0).toLong)
      last <- Try(arr(1).toDouble)
      lastDateTime <- Try(arr(2).toLong)
      volBid <- Try(arr(3).toInt)
      volAsk <- Try(arr(4).toInt)
      bid <- Try(arr(5).toDouble)
      ask <- Try(arr(6).toDouble)
      vol <- Try(arr(7).toInt)
    } yield{
        if(arr(0).length != 13){
          println(arr(0))
        }

        isBetween(last, 0, 2) && isBetween(bid, 0 , 2) && isBetween(ask, 0, 2) && isBetween(vol, 0, 20) && arr(0).length == 13 && arr(2).length == 13
    }

    result.map(identity).getOrElse(false)
  }

  def isBetween(value: Double, from: Double, to: Double) = value > from && value < to
}
