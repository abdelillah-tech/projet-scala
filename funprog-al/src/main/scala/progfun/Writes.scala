package fr.esgi.al.funprog

import fr.esgi.al.funprog._
import play.api.libs.json._
import fr.esgi.al.funprog.model._

trait Writes[A] {
  def writesJson(value: A): JsValue
  def writesCsv(value: A): String
  def writesYaml(value: A, tab: String): String
}

object Writes {

  implicit def of[A](implicit w: Writes[A]): Writes[A] = w

  implicit object WritesInt extends Writes[Int] {
    override def writesJson(value: Int): JsValue = JsNumber(value)

    override def writesCsv(value: Int): String = s"${value.toString};"

    override def writesYaml(value: Int, tab: String): String =
      s" ${value.toString}\n"
  }

  implicit object WritesString extends Writes[String] {
    override def writesJson(value: String): JsValue = JsString(value)

    override def writesCsv(value: String): String = s"${value};"

    override def writesYaml(value: String, tab: String): String =
      s" ${value.toString}\n"
  }

  implicit object WritesListOfString extends Writes[List[String]] {
    override def writesJson(value: List[String]): JsValue =
      JsArray(value.map(command => Writes.of[String].writesJson(command)))

    override def writesCsv(value: List[String]): String = s"${value.mkString}\n"

    override def writesYaml(value: List[String], tab: String): String =
      value
        .map(item => s"${tab}-${Writes.of[String].writesYaml(item, "")}")
        .mkString
  }

  implicit object WritesPoint extends Writes[Point] {
    override def writesJson(value: Point): JsValue = {
      val pointInfoMap = Map(
        "x" -> Writes.of[Int].writesJson(value.x),
        "y" -> Writes.of[Int].writesJson(value.y)
      )
      JsObject(pointInfoMap)
    }

    override def writesCsv(value: Point): String =
      s"${Writes.of[Int].writesCsv(value.x)}${Writes.of[Int].writesCsv(value.y)}"

    override def writesYaml(value: Point, tab: String): String =
      s"${tab}x:${Writes.of[Int].writesYaml(value.x, "")}" +
        s"${tab}y:${Writes.of[Int].writesYaml(value.y, "")}"
  }

  implicit object WritesPosition extends Writes[Position] {
    override def writesJson(value: Position): JsValue = {
      val positionInfoMap = Map(
        "point"     -> Writes.of[Point].writesJson(value.point),
        "direction" -> Writes.of[String].writesJson(value.direction)
      )
      JsObject(positionInfoMap)
    }

    override def writesCsv(value: Position): String =
      s"${Writes.of[Point].writesCsv(value.point)}${Writes.of[String].writesCsv(value.direction)}"

    override def writesYaml(value: Position, tab: String): String =
      s"${tab}point:\n${Writes.of[Point].writesYaml(value.point, s"${Constants.TAB}${tab}")}" +
        s"${tab}direction:${Writes.of[String].writesYaml(value.direction, "")}"
  }

  implicit object WritesTendeuse extends Writes[Tendeuse] {
    override def writesJson(value: Tendeuse): JsValue = {
      val tendeuseInfoMap = Map(
        "debut"        -> Writes.of[Position].writesJson(value.debut),
        "instructions" -> Writes.of[List[String]].writesJson(value.instruction),
        "fin"          -> Writes.of[Position].writesJson(value.fin)
      )
      JsObject(tendeuseInfoMap)
    }

    override def writesCsv(value: Tendeuse): String =
      s"${Writes.of[Position].writesCsv(value.debut)}" +
        s"${Writes.of[Position].writesCsv(value.fin)}" +
        s"${Writes.of[List[String]].writesCsv(value.instruction)}"

    override def writesYaml(value: Tendeuse, tab: String): String =
      s"- debut:\n${Writes.of[Position].writesYaml(value.debut, s"${Constants.TAB}${tab}")}" +
        s"${tab}instructions:\n${Writes.of[List[String]].writesYaml(value.instruction, s"${tab}")}" +
        s"${tab}fin:\n${Writes.of[Position].writesYaml(value.fin, s"${Constants.TAB}${tab}")}"
  }

  implicit object WritesResult extends Writes[Result] {
    override def writesJson(value: Result): JsValue =
      JsObject(
        Map(
          "limite" -> Writes.of[Point].writesJson(value.limit),
          "tondeuses" -> JsArray(
            value.tendeuses.map(
              tendeuse => Writes.of[Tendeuse].writesJson(tendeuse)
            )
          )
        )
      )
    override def writesCsv(value: Result): String =
      "numéro;début_x;début_y;début_direction;fin_x;fin_y;fin_direction;instructions\n" +
        s"${value.tendeuses.zipWithIndex
          .map(tendeuse => {
            Writes.of[Int].writesCsv(tendeuse._2 + 1) + Writes.of[Tendeuse].writesCsv(tendeuse._1)
          })
          .mkString}"

    override def writesYaml(value: Result, tab: String): String = {
      s"limite:\n${Writes.of[Point].writesYaml(value.limit, s"${Constants.TAB}${tab}")}" +
        s"tondeuses:\n${value.tendeuses
          .map(tendeuse => s"${Writes.of[Tendeuse].writesYaml(tendeuse, Constants.TAB)}")
          .mkString}"
    }
  }
}
