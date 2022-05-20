package ucesoft.nes.util

import java.io.{FileInputStream, InputStream}
import java.util.zip.GZIPInputStream
import scala.collection.mutable.ListBuffer

object Cheat:
  def main(args:Array[String]): Unit =
    //println(gameGenieDecode(io.StdIn.readLine("Cheat code:")))
    for e <- cheatsDB do
      println(s"${e._1}\t\t${e._2}")

  case class CheatDBEntry(game:String,codes:List[CheatDBEntryEffect])
  case class CheatDBEntryEffect(codes:List[String],effect:String)

  trait CheatManager:
    def addCheat(cheat:Cheat): Unit
    def removeCheat(cheat:Cheat): Unit
    def removeAllCheats(): Unit
    def getCheats(): List[Cheat]

  case class Cheat(address:Int,data:Int,compare:Option[Int],code:String):
    override def toString: String =
      compare match
        case Some(c) =>
          s"${address.toHexString.toUpperCase()}?${c.toHexString.toUpperCase()}:${data.toHexString.toUpperCase()}"
        case None =>
          s"${address.toHexString.toUpperCase()}:${data.toHexString.toUpperCase()}"

  val cheatsDB : Map[String,CheatDBEntry] = buildCheatsDB(new GZIPInputStream(getClass.getResourceAsStream("/resources/cheatdb/gamegenieDB.csv.gz")))

  private final val GAME_GENIE_TABLE = Map('A' -> 0,'P' -> 1,'Z' -> 2,'L' -> 3,'G' -> 4,'I' -> 5,'T' -> 6,'Y' -> 7,'E' -> 8,'O' -> 9,'X' -> 10,'U' -> 11,'K' -> 12,'S' -> 13,'V' -> 14,'N' -> 15)

  private def buildCheatsDB(in:InputStream): Map[String,CheatDBEntry] =
    val map = new collection.mutable.LinkedHashMap[String,CheatDBEntry]
    val lines = io.Source.fromInputStream(in,"UTF-8").getLines()
    var lastGame = ""
    var lastCodes = new ListBuffer[CheatDBEntryEffect]
    for line <- lines do
      val fields = line.split(";")
      if fields.length > 3 && !fields(1).isEmpty then
        val game = fields(0)
        val codes = fields(2).split("\\+").map(_.trim).toList
        val effect = fields(3)
        if lastGame.isEmpty then
          lastGame = game
        if game != lastGame then
          map += lastGame.toUpperCase() -> CheatDBEntry(lastGame,lastCodes.toList)
          lastGame = game
          lastCodes = new ListBuffer[CheatDBEntryEffect]
        lastCodes += CheatDBEntryEffect(codes,effect)
    in.close()
    map.toMap

  def gameGenieDecode(code:String): Option[Cheat] =
    def bit(v:Int,b:Int): Int = if (v & (1 << b)) > 0 then 1 else 0

    if code.length != 6 && code.length != 8 then return None

    val hex = code map { c =>
      GAME_GENIE_TABLE.get(c) match
        case None =>
          return None
        case Some(v) => v
    }

    val data = bit(hex(0),0) |
               bit(hex(0),1) << 1 |
               bit(hex(0),2) << 2 |
               (if hex.length == 6 then bit(hex(5),3) else bit(hex(7),3)) << 3 |
               bit(hex(1),0) << 4 |
               bit(hex(1),1) << 5 |
               bit(hex(1),2) << 6 |
               bit(hex(0),3) << 7

    val address =
      bit(hex(4),0) |
      bit(hex(4),1)  << 1 |
      bit(hex(4),2)  << 2 |
      bit(hex(3),3)  << 3 |
      bit(hex(2),0)  << 4 |
      bit(hex(2),1)  << 5 |
      bit(hex(2),2)  << 6 |
      bit(hex(1),3)  << 7 |
      bit(hex(5),0)  << 8 |
      bit(hex(5),1)  << 9 |
      bit(hex(5),2)  << 10 |
      bit(hex(4),3)  << 11 |
      bit(hex(3),0)  << 12 |
      bit(hex(3),1)  << 13 |
      bit(hex(3),2)  << 14 |
      1 << 15

    val compare =
      if hex.length == 6 then None
      else
        Some(
          bit(hex(6),0) |
          bit(hex(6),1)  << 1 |
          bit(hex(6),2)  << 2 |
          bit(hex(5),3)  << 3 |
          bit(hex(7),0)  << 4 |
          bit(hex(7),1)  << 5 |
          bit(hex(7),2)  << 6 |
          bit(hex(6),3)  << 7
        )

    Some(Cheat(address,data,compare,code))
