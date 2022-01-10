package fr.esgi.al.funprog

import fr.esgi.al.funprog.model._

object Controleur {
  def move(
      position: Position,
      commands: List[String],
      limit: Point
  ): Position = commands match {
    case Nil => position
    case el :: rest if el == "D" || el == "G" =>
      move(
        Position(
          Point(position.point.x, position.point.y),
          direction(position.direction, el)
        ),
        rest,
        limit
      )
    case el :: rest if el == "A" =>
      move(advance(position, limit), rest, limit)
    case _ => move(position, commands, limit)
  }

  def direction(orientation: String, command: String): String =
    orientation match {
      case "N" => if (command == "G") "W" else "E"
      case "E" => if (command == "G") "N" else "S"
      case "W" => if (command == "G") "S" else "N"
      case "S" =>
        if (command == "G") "E" else "W"
      case _ => orientation
    }

  def advance(
      position: Position,
      limit: Point
  ): Position = position.direction match {
    case "N" if (position.point.y + 1 <= limit.y) =>
      Position(
        Point(position.point.x, position.point.y + 1),
        position.direction
      )
    case "E" if (position.point.x + 1 <= limit.x) =>
      Position(
        Point(position.point.x + 1, position.point.y),
        position.direction
      )
    case "W" if (position.point.x - 1 >= 0) =>
      Position(
        Point(position.point.x - 1, position.point.y),
        position.direction
      )
    case "S" if (position.point.y - 1 >= 0) =>
      Position(
        Point(position.point.x, position.point.y - 1),
        position.direction
      )
    case _ => position
  }

  def designate(
      parsedData: ParsedData
  ): List[Tendeuse] = parsedData.tendeusesDataInput match {
    case Nil => Nil
    case el :: rest => {
      val fin: Position = move(el.position, el.instruction, parsedData.limit)
      Tendeuse(el.position, el.instruction, fin) :: designate(
        ParsedData(rest, parsedData.limit)
      )
    }
    case _ => Nil
  }
}
