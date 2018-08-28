import Command.Command
import Direction.Direction

object MarsRover {

  def receive(current: (Position, Direction), commands: Seq[Command]): (Position, Direction) = {
    move(current, commands.head)
  }

  def move(current: (Position, Direction), command: Command): (Position, Direction) = {
    val currentPosition = current._1
    val currentDirection = current._2
    (currentDirection, command) match {
      case (Direction.EAST, Command.FORWARD) | (Direction.WEST, Command.BACKWARD) =>
        (Position(currentPosition.x + 1, currentPosition.y), currentDirection)
      case (Direction.WEST, Command.FORWARD) | (Direction.EAST, Command.BACKWARD)=>
        (Position(currentPosition.x - 1, currentPosition.y), currentDirection)
      case (Direction.SOUTH, Command.FORWARD) | (Direction.NORTH, Command.BACKWARD)=>
        (Position(currentPosition.x, currentPosition.y + 1), currentDirection)
      case (Direction.NORTH, Command.FORWARD) | (Direction.SOUTH, Command.BACKWARD) =>
        (Position(currentPosition.x, currentPosition.y - 1), currentDirection)
      case _ =>
        (currentPosition, currentDirection)
    }
  }
}

case class Position(x: Int, y: Int)

object Direction extends Enumeration {
  type Direction = Value
  val NORTH = Value("N")
  val EAST = Value("E")
  val SOUTH = Value("S")
  val WEST = Value("W")
}

object Command extends Enumeration {
  type Command = Value
  val FORWARD = Value("f")
  val BACKWARD = Value("b")
  val LEFT = Value("l")
  val RIGHT = Value("r")
}
