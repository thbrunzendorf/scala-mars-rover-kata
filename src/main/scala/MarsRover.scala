import scala.annotation.tailrec

object MarsRover {

  @tailrec
  def receive(current: (Position, Direction), commands: Seq[Command]): (Position, Direction) = {
    commands.length match {
      case 0 => current
      case _ => {
        receive(computeExactlyOneOfNewPositionOrNewDirection(current, commands.head), commands.tail)
      }
    }
  }

  def computeExactlyOneOfNewPositionOrNewDirection(current: (Position, Direction), command: Command): (Position, Direction) = {
    val currentPosition = current._1
    val currentDirection = current._2
    (currentDirection, command) match {
      case (East, Forward) | (West, Backward) =>
        (Position(currentPosition.x + 1, currentPosition.y), currentDirection)
      case (West, Forward) | (East, Backward)=>
        (Position(currentPosition.x - 1, currentPosition.y), currentDirection)
      case (South, Forward) | (North, Backward)=>
        (Position(currentPosition.x, currentPosition.y - 1), currentDirection)
      case (North, Forward) | (South, Backward) =>
        (Position(currentPosition.x, currentPosition.y + 1), currentDirection)

      case (anyDirection, Left) =>
        (currentPosition, anyDirection.left())
      case (anyDirection, Right) =>
        (currentPosition, anyDirection.right())

      case _ =>
        (currentPosition, currentDirection)
    }
  }
}

case class Position(x: Int, y: Int)

abstract class Direction() {
  def left(): Direction
  def right(): Direction
  def opposite(): Direction
}

object North extends Direction {
  def left(): Direction = {
    West
  }
  def right(): Direction = {
    East
  }
  override def opposite() = South
}

object West extends Direction {
  def left(): Direction = {
    South
  }
  def right(): Direction = {
    North
  }
  override def opposite() = East
}

object South extends Direction {
  def left(): Direction = {
    East
  }
  def right(): Direction = {
    West
  }
  override def opposite() = North
}

object East extends Direction {
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
}

object Forward extends Command {
  override def opposite() = Backward
}
object Backward extends Command {
  override def opposite() = Forward
}
object Left extends Command {
  override def opposite() = Right
}
object Right extends Command {
  override def opposite() = Left
}
