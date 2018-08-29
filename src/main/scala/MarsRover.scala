import Command.Command
import Direction.Direction

object MarsRover {

  def receive(current: (Position, NotDirectionYet), commands: Seq[Command]): (Position, Direction) = {
    val currentPosition = current._1
    val currentNotDirectionYet = current._2
    currentNotDirectionYet match {
      case North => move((currentPosition, North), commands.head)
      case  East => move((currentPosition, East), commands.head)
      case South => move((currentPosition, South), commands.head)
      case  West => move((currentPosition, West), commands.head)
    }
  }

  def move(current: (Position, NotDirectionYet), command: Command): (Position, Direction) = {
    val currentPosition = current._1
    val currentDirection = current._2
    (currentDirection, command) match {
      case (East, Command.FORWARD) | (West, Command.BACKWARD) =>
        (Position(currentPosition.x + 1, currentPosition.y), currentDirection.mapToDirection())
      case (West, Command.FORWARD) | (East, Command.BACKWARD)=>
        (Position(currentPosition.x - 1, currentPosition.y), currentDirection.mapToDirection())
      case (South, Command.FORWARD) | (North, Command.BACKWARD)=>
        (Position(currentPosition.x, currentPosition.y + 1), currentDirection.mapToDirection())
      case (North, Command.FORWARD) | (South, Command.BACKWARD) =>
        (Position(currentPosition.x, currentPosition.y - 1), currentDirection.mapToDirection())

      case (South, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), South.left().mapToDirection())
      case (North, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), North.left().mapToDirection())
      case (West, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), West.left().mapToDirection())
      case (East, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), East.left().mapToDirection())

      case _ =>
        (currentPosition, currentDirection.mapToDirection())
    }
  }
}

case class Position(x: Int, y: Int)

object Direction extends Enumeration {
  type Direction = Value
  val NORTH = Value("N")
  val  EAST = Value("E")
  val SOUTH = Value("S")
  val  WEST = Value("W")
}

abstract class NotDirectionYet() {
  val NORTH = "N"
  val  EAST = "E"
  val SOUTH = "S"
  val  WEST = "W"
  def left(): NotDirectionYet
  def right(): NotDirectionYet
  def mapToDirection(): Direction = {
    this match {
      case (North) => Direction.NORTH
      case (East) => Direction.EAST
      case (South) => Direction.SOUTH
      case (West) => Direction.WEST
    }
  }
}

object North extends NotDirectionYet {
  def left(): NotDirectionYet = {
    return West
  }
  def right(): NotDirectionYet = {
    return East
  }
}

object West extends NotDirectionYet {
  def left(): NotDirectionYet = {
    return South
  }
  def right(): NotDirectionYet = {
    return North
  }
}

object South extends NotDirectionYet {
  def left(): NotDirectionYet = {
    return East
  }
  def right(): NotDirectionYet = {
    return West
  }
}

object East extends NotDirectionYet {
  def left(): NotDirectionYet = {
    return North
  }
  def right(): NotDirectionYet = {
    return South
  }
}

object Command extends Enumeration {
  type Command = Value
  val  FORWARD = Value("f")
  val BACKWARD = Value("b")
  val     LEFT = Value("l")
  val    RIGHT = Value("r")
}
