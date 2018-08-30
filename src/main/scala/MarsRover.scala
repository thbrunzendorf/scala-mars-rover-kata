import scala.annotation.tailrec

object MarsRover {

  @tailrec
  def receive(current: (Position, Direction), commands: Seq[Command]): (Position, Direction) = {
    commands.length match {
      case 0 => current
      case _ => {
        receive(executeCommand(current, commands.head), commands.tail)
      }
    }
  }

  def executeCommand(current: (Position, Direction), command: Command): (Position, Direction) = {
    command.execute(current)
  }
}

case class Position(x: Int, y: Int)

abstract class Direction() {
  def left(): Direction
  def right(): Direction
  def opposite(): Direction
  def increment(position: Position): Position
}

object North extends Direction {
  override def increment(position: Position): Position = {
    Position(position.x, position.y + 1)
  }
  def left(): Direction = {
    West
  }
  def right(): Direction = {
    East
  }
  override def opposite() = South
}

object West extends Direction {
  override def increment(position: Position): Position = {
    Position(position.x - 1, position.y)
  }

  def left(): Direction = {
    South
  }
  def right(): Direction = {
    North
  }
  override def opposite() = East
}

object South extends Direction {
  override def increment(position: Position): Position = {
    Position(position.x, position.y - 1)
  }
  def left(): Direction = {
    East
  }
  def right(): Direction = {
    West
  }
  override def opposite() = North
}

object East extends Direction {
  def increment(position: Position): Position = {
    Position(position.x + 1, position.y)
  }

  def left(): Direction = {
    North
  }
  def right(): Direction = {
    South
  }
  override def opposite() = West
}

abstract class Command {
  def opposite(): Command
  def execute(current: (Position, Direction)): (Position, Direction)
}

object Forward extends Command {
  override def opposite() = Backward

  override def execute(current: (Position, Direction)): (Position, Direction) = {
    val (currentPosition, currentDirection) = current
    (currentDirection.increment(currentPosition), currentDirection)
  }
}
object Backward extends Command {
  override def opposite() = Forward

  override def execute(current: (Position, Direction)): (Position, Direction) = {
    val (currentPosition, currentDirection) = current
    (currentDirection.opposite().increment(currentPosition), currentDirection)
  }
}
object Left extends Command {
  override def opposite() = Right

  override def execute(current: (Position, Direction)): (Position, Direction) = (current._1, current._2.left())
}
object Right extends Command {
  override def opposite() = Left

  override def execute(current: (Position, Direction)): (Position, Direction) = (current._1, current._2.right())
}
