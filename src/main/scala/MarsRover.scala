import Command.Command

object MarsRover {

  def receive(current: (Position, NotDirectionYet), commands: Seq[Command]): (Position, NotDirectionYet) = {
    move(current, commands.head)
  }

  def move(current: (Position, NotDirectionYet), command: Command): (Position, NotDirectionYet) = {
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

abstract class NotDirectionYet() {
  val NORTH = "N"
  val  EAST = "E"
  val SOUTH = "S"
  val  WEST = "W"
  def left(): NotDirectionYet
  def right(): NotDirectionYet
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
