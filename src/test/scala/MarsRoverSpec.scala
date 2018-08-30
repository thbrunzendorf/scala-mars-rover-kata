
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
        Left,
        Forward,
        Left,
      )
      val (position, direction) = MarsRover.receive((Position(1,1), East), several_commands)
      position shouldEqual Position(1,2)
      direction shouldEqual West
    }

    "turn orientation east when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), South), Seq(Left))
      position shouldEqual Position(1,1)
      direction shouldEqual East
    }
    "turn orientation west when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North), Seq(Left))
      position shouldEqual Position(1,1)
      direction shouldEqual West
    }
    "turn orientation south when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), West), Seq(Left))
      position shouldEqual Position(1,1)
      direction shouldEqual South
    }
    "turn orientation north when receiving left command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), East), Seq(Left))
      position shouldEqual Position(1,1)
      direction shouldEqual North
    }

    "turn orientation east when receiving right command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North), Seq(Right))
      position shouldEqual Position(1,1)
      direction shouldEqual East
    }

    "advance position eastwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), East), Seq(Forward))
      position shouldEqual Position(2,1)
      direction shouldEqual East
    }
    "advance position westwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), West), Seq(Forward))
      direction shouldEqual West
      position shouldEqual Position(0,1)
    }
    "advance position southwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), South), Seq(Forward))
      direction shouldEqual South
      position shouldEqual Position(1,0)
    }
    "advance position northwards when receiving forward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North),Seq(Forward))
      direction shouldEqual North
      position shouldEqual Position(1,2)
    }

    "advance position eastwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), East),Seq(Backward))
      direction shouldEqual East
      position shouldEqual Position(0,1)
    }
    "advance position westwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), West),Seq(Backward))
      direction shouldEqual West
      position shouldEqual Position(2,1)
    }
    "advance position southwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), South),Seq(Backward))
      direction shouldEqual South
      position shouldEqual Position(1,2)
    }
    "advance position northwards when receiving backward command" in {
      val (position, direction) = MarsRover.receive((Position(1,1), North),Seq(Backward))
      direction shouldEqual North
      position shouldEqual Position(1,0)
    }
  }
}
