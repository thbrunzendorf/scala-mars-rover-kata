import Command.Command

object MarsRover {

  def receive(current: (Position, Direction), commands: Seq[Command]): (Position, Direction) = {
    move(current, commands.head)
  }

  def move(current: (Position, Direction), command: Command): (Position, Direction) = {
    val currentPosition = current._1
    val currentDirection = current._2
    (currentDirection, command) match {
      case (East, Command.FORWARD) | (West, Command.BACKWARD) =>
        (Position(currentPosition.x + 1, currentPosition.y), currentDirection)
      case (West, Command.FORWARD) | (East, Command.BACKWARD)=>
        (Position(currentPosition.x - 1, currentPosition.y), currentDirection)
      case (South, Command.FORWARD) | (North, Command.BACKWARD)=>
        (Position(currentPosition.x, currentPosition.y + 1), currentDirection)
      case (North, Command.FORWARD) | (South, Command.BACKWARD) =>
        (Position(currentPosition.x, currentPosition.y - 1), currentDirection)

      case (South, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), South.left())
      case (North, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), North.left())
      case (West, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), West.left())
      case (East, Command.LEFT) =>
        (Position(currentPosition.x, currentPosition.y), East.left())

      case _ =>
        (currentPosition, currentDirection)
    }
  }
}

case class Position(x: Int, y: Int)

abstract class Direction() {
  def left(): Direction
  def right(): Direction
}

object North extends Direction {
  def left(): Direction = {
    return West
  }
  def right(): Direction = {
    return East
  }
}

object West extends Direction {
  def left(): Direction = {
    return South
  }
  def right(): Direction = {
    return North
  }
}

object South extends Direction {
  def left(): Direction = {
    return East
  }
  def right(): Direction = {
    return West
  }
}

object East extends Direction {
  def left(): Direction = {
    return North
  }
  def right(): Direction = {
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
