package fr.esgi.al.funprog

import java.io.{FileNotFoundException, IOException}
import com.typesafe.config.{Config, ConfigFactory}
import better.files._
import fr.esgi.al.funprog.model._

object Parseur {
  @throws(classOf[IOException])
  @throws(classOf[FileNotFoundException])
  def readInfos(): List[List[String]] = {
    val conf: Config = ConfigFactory.load()
    val path: String = conf.getString("application.input-file")
    val f = File(path)
    val lines = f.lines.toList
    lines.map(line => line.split("").map(_.trim).toList)
  }

  def parser(
      data: List[List[String]]
  ): ParsedData = {
    val limit: Point = Point(data(0)(0).toInt, data(0)(2).toInt)
    val poses: List[Position] = data.zipWithIndex
      .filter((element: Tuple2[List[String], Int]) => element._2 % 2 == 1)
      .map(element => {
        val point: Point = Point(element._1(0).toInt, element._1(2).toInt)
        val direction: String = element._1(4)
        val position: Position = Position(point, direction)
        position
      })
    val commands: List[List[String]] = data.zipWithIndex
      .filter(
        (element: Tuple2[List[String], Int]) =>
          element._2 % 2 == 0 && element._2 != 0
      )
      .map(element => element._1)

    val infos: List[TendeuseDataInput] =
      poses.zip(commands).map(info => TendeuseDataInput(info._1, info._2))
    val parsed: ParsedData = ParsedData(infos, limit)
    parsed
  }
}
