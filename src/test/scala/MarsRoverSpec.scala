
import org.scalatest.{Matchers, WordSpec}

class MarsRoverSpec extends WordSpec with Matchers {

  "MarsRover" should {
    "do nothing with an empty sequence" in {
      val (position, direction) = MarsRover.receive((Position(1,1), South), Seq.empty)
      position shouldEqual Position(1,1)
      direction shouldEqual South
    }

    "do several commands in the order given" in {
      val several_commands = Seq(
        Command.LEFT,
        Command.FORWARD,
        Command.LEFT,
      )
      val (position, direction) = MarsRover.receive((Position(1,1), East), several_commands)
      position shouldEqual Position(1,2)
      direction shouldEqual West
    }

    "turn orientation east when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), South), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual East
    }
    "turn orientation west when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual West
    }
    "turn orientation south when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), West), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual South
    }
    "turn orientation north when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), East), Seq(Command.LEFT))
      position shouldEqual Position(1,1)
      direction shouldEqual North
    }

    "turn orientation east when receiving right command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North), Seq(Command.RIGHT))
      position shouldEqual Position(1,1)
      direction shouldEqual East
    }

    "advance position eastwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), East), Seq(Command.FORWARD))
      position shouldEqual Position(2,1)
      direction shouldEqual East
    }
    "advance position westwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), West), Seq(Command.FORWARD))
      direction shouldEqual West
      position shouldEqual Position(0,1)
    }
    "advance position southwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), South), Seq(Command.FORWARD))
      direction shouldEqual South
      position shouldEqual Position(1,0)
    }
    "advance position northwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North),Seq(Command.FORWARD))
      direction shouldEqual North
      position shouldEqual Position(1,2)
    }

    "advance position eastwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), East),Seq(Command.BACKWARD))
      direction shouldEqual East
      position shouldEqual Position(0,1)
    }
    "advance position westwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), West),Seq(Command.BACKWARD))
      direction shouldEqual West
      position shouldEqual Position(2,1)
    }
    "advance position southwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), South),Seq(Command.BACKWARD))
      direction shouldEqual South
      position shouldEqual Position(1,2)
    }
    "advance position northwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North),Seq(Command.BACKWARD))
      direction shouldEqual North
      position shouldEqual Position(1,0)
    }
  }
}

// TODO pbt
// FORWARD / BACKWARD only change position => "positional commands"
// LEFT / RIGHT only change direction => "directional commands"
// (direction, command) has same effect as (opposite direction, opposite command)
