package ucesoft.nes.util

import org.w3c.dom.{Element, Node}
import org.yaml.snakeyaml.Yaml
import ucesoft.nes.{Cartridge, PPU}
import ucesoft.nes.controller.InputType
import ucesoft.nes.mappers.MapperFactory
import scala.jdk.CollectionConverters.*
import java.util.{List as JList, Map as JMap}
import java.io.{BufferedInputStream, DataInputStream, EOFException, FileInputStream}
import java.util.zip.GZIPInputStream
import javax.xml.parsers.DocumentBuilderFactory
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CartDB:
  case class Game(name:String,
                  developer:String,
                  date:String, crc:Long,
                  mapper:Int,
                  mapperType:String,
                  pcb:String,
                  wramSizeK:Option[Int],
                  vramSizeK:Option[Int],
                  padH:Option[Int],
                  devices:List[InputType],
                  chipsType:List[String],
                  tv:String,
                  vsGame:Option[VSGame] = None):
    def isVSGame:Boolean = vsGame.isDefined
    override def toString = s"Game(name=$name,dev=$developer,date=$date,crc=$crc,mapper=$mapper,mapperType=$mapperType,pcb=$pcb,wram=${wramSizeK.getOrElse(0)},vram=${vramSizeK.getOrElse(0)},padH=${padH.getOrElse(0)},devs=$devices,chipsType=$chipsType,tv=$tv,vsGame=${vsGame.map(_.toString).getOrElse("")})"

  case class VSSwitch(label:String, switch:Int)
  case class VSSwitches(label:String, activeIndex:Int, switches:Array[VSSwitch])
  case class VSGame(name:String,
                    mapper:Int,
                    mirroring:Cartridge.Mirroring,
                    ppuType:PPU.PPUType,
                    swapControllers:Boolean,
                    zapper:Boolean,
                    switches:List[VSSwitches],
                    ppuStatusCode:Option[Int],
                    securityCode:Option[String]):
    lazy val switchValues : Int = switches.map(s => s.switches(s.activeIndex).switch).reduce(_ | _)
    override def toString: String = s"VSGame($name,$mapper,$mirroring,$ppuType)"

  val gameDB : Map[Long,Game] = loadGames()
  val coverage : Double = gameDB.values.count(g => MapperFactory.mappers.contains(g.mapper)) / gameDB.size.toDouble * 100.0

  private def loadVSGames(): Map[Long,VSGame] =
    val yaml = new Yaml()
    val db = yaml.load[JList[JMap[String,AnyRef]]](getClass.getResourceAsStream("/resources/cartdb/vsgames.yaml")).asScala
    val games = new collection.mutable.HashMap[Long,VSGame]

    for g <- db do
      val game = g.asScala.head._2.asInstanceOf[JMap[String,AnyRef]].asScala
      val name = game("label").toString
      val mapper = game("mapper").asInstanceOf[Int]
      val mirroring = Cartridge.Mirroring.valueOf(game("mirroring").toString)
      val ppuType = PPU.PPUType.valueOf("_" + game("ppu").toString)
      val ppuStatusCode = ppuType match
        case PPU.PPUType._2C05 =>
          Some(game("ppu-status").asInstanceOf[Int])
        case _ =>
          None
      val swapControllers = game("swap_controllers").toString.toUpperCase() == "TRUE"
      val zapper = game("zapper").toString.toUpperCase() == "TRUE"
      val securityCode = game.get("security").map(_.toString.toUpperCase())
      val crc = game("crc").asInstanceOf[JList[Number]].asScala.toList

      val switchList = new ListBuffer[VSSwitches]
      val switches = game("dip_switches").asInstanceOf[JList[JMap[String,AnyRef]]].asScala
      for s <- switches do
        val switch = s.asScala.head._2.asInstanceOf[JMap[String,AnyRef]].asScala
        val switchName = switch("label").toString
        val defValue = switch("default").asInstanceOf[Int]
        val switchConf = switch("switches").asInstanceOf[JList[AnyRef]].asScala.sliding(2,2)
        val sws = for sc <- switchConf yield
          val conf = sc(0).toString
          val sw = sc(1).asInstanceOf[Int]
          VSSwitch(conf,sw)
        switchList += VSSwitches(switchName,defValue,sws.toArray)
      for c <- crc do
        games += c.longValue() -> VSGame(name,mapper,mirroring,ppuType,swapControllers,zapper,switchList.toList,ppuStatusCode,securityCode)

    games.toMap

  private def loadGames(): Map[Long,Game] =
    try
      val dbf = DocumentBuilderFactory.newInstance()
      val db = dbf.newDocumentBuilder()
      val in = new GZIPInputStream(getClass().getResourceAsStream("/resources/cartdb/nescartdb.xml.gz"))
      val doc = db.parse(in)
      in.close()

      doc.getDocumentElement().normalize()
      val map = new mutable.HashMap[Long,Game]
      val games = doc.getElementsByTagName("game")

      for(i <- 0 until games.getLength) {
        val game = games.item(i).asInstanceOf[Element]
        val gameName = game.getAttribute("name")
        val developer = game.getAttribute("developer")
        val date = game.getAttribute("date")
        val devices = game.getElementsByTagName("peripherals")
        val deviceList = if devices.getLength > 0 then
          val devs = devices.item(0).asInstanceOf[Element].getElementsByTagName("device")
          val devNames = (for(p <- 0 until devices.getLength) yield devs.item(p).asInstanceOf[Element].getAttribute("type")).toList
          devNames map { d => InputType.values.find(i => i.nameType == d) } flatten
        else List()
        val carts = game.getElementsByTagName("cartridge")
        for(c <- 0 until carts.getLength) {
          val cart = carts.item(c).asInstanceOf[Element]
          val crc = java.lang.Long.parseLong(cart.getAttribute("crc"),16)
          val tv = cart.getAttribute("system")
          if !map.contains(crc) then
            val board = cart.getElementsByTagName("board").item(0).asInstanceOf[Element]
            val mapper = board.getAttribute("mapper").toInt
            val mapperType = board.getAttribute("type")
            val pcb = board.getAttribute("pcb")
            val chips = board.getElementsByTagName("chip")
            val chipsType = (for(c <- 0 until chips.getLength) yield chips.item(c).asInstanceOf[Element].getAttribute("type")).toList
            val wrams = board.getElementsByTagName("wram")
            var wramSize = 0
            for r <- 0 until wrams.getLength do
              val wram = wrams.item(r).asInstanceOf[Element]
              val size = if wram == null then 0 else wram.getAttribute("size").dropRight(1).toInt
              wramSize += size
            val vrams = board.getElementsByTagName("vram")
            var vramSize = 0
            for r <- 0 until vrams.getLength do
              val vram = vrams.item(r).asInstanceOf[Element]
              val size = if vram == null then 0 else vram.getAttribute("size").dropRight(1).toInt
              vramSize += size
            val pad = board.getElementsByTagName("pad").item(0).asInstanceOf[Element]
            val padInfo = if pad == null then None else Some(pad.getAttribute("h").toInt)
            map.put(crc,
              Game(gameName,
                developer,
                date,
                crc,
                mapper,
                mapperType,
                pcb,
                if wramSize > 0 then Some(wramSize) else None,
                if vramSize > 0 then Some(vramSize) else None,
                padInfo,
                deviceList,
                chipsType,
                tv)
            )
        }
      }

      val vsGames : Map[Long,VSGame] = loadVSGames()
      for (crc,vsg) <- vsGames do
        map += crc -> Game(vsg.name,"Unknown","",crc,vsg.mapper,"Unknown","",None,None,None,if vsg.zapper then List(InputType.VSZapper) else Nil,Nil,"",Some(vsg))
      map.toMap
    catch
      case t:Throwable =>
        t.printStackTrace()
        Map()

  def main(args:Array[String]): Unit =
    val map = gameDB.values.groupBy(game => game.mapper).map(kv => (kv._1,kv._2.size))
    val perc = for kv <- map.keys.toArray yield (kv,map(kv),map(kv).toDouble / gameDB.size * 100)
    for e <- perc.sortBy(-_._2) do
      println(s"${e._1}\t\t${e._2}[${e._3} %]")
