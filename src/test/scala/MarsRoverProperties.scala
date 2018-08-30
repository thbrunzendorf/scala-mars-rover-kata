import Command.Command
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object MarsRoverProperties extends Properties("MarsRover") {
  val validPosition = for {
    n <- Gen.choose(-5,5)
    m <- Gen.choose(-5,5)
  } yield Position(n,m)

  val validDirection = for {
    direction <- Gen.oneOf(North, South, East, West)
  } yield direction

  // TODO: one of validTurnCommand or the other kind
  val validCommand = for {
    name <- Gen.oneOf("f", "b", "l", "r")
  } yield Command.withName(name)

  val validTurnCommand = for {
    name <- Gen.oneOf("l", "r")
  } yield Command.withName(name)

  property("generate only valid positions") =
    forAll(validPosition) { (myPosition: Position) =>

    true
  }

  property("generate only valid commands") =
    forAll(validCommand) { (myCommand: Command) =>

    true
  }

  property("generate only valid directions") =
    forAll(validDirection) { (myDirection: Direction) =>

    true
  }

  property("move in any direction by at most 1") =
    forAll(validPosition, validDirection, validCommand) { (myPosition: Position, myDirection: Direction, myCommand: Command) =>

      val (actualPosition, _) = MarsRover.move((myPosition, myDirection), myCommand)
      math.abs(actualPosition.x - myPosition.x) <= 1 &&
        math.abs(actualPosition.y - myPosition.y) <= 1
  }

  property("turning doesn't move") =
    forAll(validPosition, validDirection, validTurnCommand) { (myPosition: Position, myDirection: Direction, myTurnCommand: Command) =>

      val (actualPosition, actualDirection) = MarsRover.move((myPosition, myDirection), myTurnCommand)
      actualPosition.x == myPosition.x &&
        actualPosition.y == myPosition.y &&
        actualDirection != myDirection
  }
}
