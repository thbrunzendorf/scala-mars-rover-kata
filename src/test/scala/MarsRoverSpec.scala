
import org.scalatest.{Matchers, WordSpec}

class MarsRoverSpec extends WordSpec with Matchers {

  "MarsRover" should {
    "advance position eastwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.EAST), Seq(Command.FORWARD))
      position shouldEqual Position(2,1)
      direction shouldEqual Direction.EAST
    }
    "advance position westwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.WEST), Seq(Command.FORWARD))
      direction shouldEqual Direction.WEST
      position shouldEqual Position(0,1)
    }
    "advance position southwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.SOUTH), Seq(Command.FORWARD))
      direction shouldEqual Direction.SOUTH
      position shouldEqual Position(1,2)
    }
    "advance position northwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.NORTH),Seq(Command.FORWARD))
      direction shouldEqual Direction.NORTH
      position shouldEqual Position(1,0)
    }

    "advance position eastwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.EAST),Seq(Command.BACKWARD))
      direction shouldEqual Direction.EAST
      position shouldEqual Position(0,1)
    }
    "advance position westwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.WEST),Seq(Command.BACKWARD))
      direction shouldEqual Direction.WEST
      position shouldEqual Position(2,1)
    }
    "advance position southwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.SOUTH),Seq(Command.BACKWARD))
      direction shouldEqual Direction.SOUTH
      position shouldEqual Position(1,0)
    }
    "advance position northwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), Direction.NORTH),Seq(Command.BACKWARD))
      direction shouldEqual Direction.NORTH
      position shouldEqual Position(1,2)
    }
  }
}

// TODO pbt
// FORWARD / BACKWARD only change position => "positional commands"
// LEFT / RIGHT only change direction => "directional commands"
// (direction, command) has same effect as (opposite direction, opposite command)
