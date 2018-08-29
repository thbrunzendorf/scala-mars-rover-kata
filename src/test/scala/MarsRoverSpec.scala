
import org.scalatest.{Matchers, WordSpec}

class MarsRoverSpec extends WordSpec with Matchers {

  "MarsRover" should {
    "turn orientation east when receiving left command" in {
      val (position, direction) = MarsRover.receiveNotDirectionYet((Position(1,1), South), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual Direction.EAST
    }
    "turn orientation west when receiving left command" in {
      val (position, direction) = MarsRover.receiveNotDirectionYet((Position(1,1), North), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual Direction.WEST
    }
    "turn orientation south when receiving left command" in {
      val (position, direction) = MarsRover.receiveNotDirectionYet((Position(1,1), West), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual Direction.SOUTH
    }
    "turn orientation north when receiving left command" in {
      val (position, direction) = MarsRover.receiveNotDirectionYet((Position(1,1), East), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual Direction.NORTH
    }

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
