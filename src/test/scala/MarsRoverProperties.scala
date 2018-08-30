import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object MarsRoverProperties extends Properties("MarsRover") {
  val validPosition = for {
    n <- Gen.choose(-5, 5)
    m <- Gen.choose(-5, 5)
  } yield Position(n, m)

  val validDirection = for {
    direction <- Gen.oneOf(North, South, East, West)
  } yield direction

  val validCommand = for {
    name <- Gen.oneOf(Forward, Backward, Left, Right)
  } yield name

  val validTurnCommand = for {
    name <- Gen.oneOf(Left, Right)
  } yield name

  val validMoveCommand = for {
    name <- Gen.oneOf(Forward, Backward)
  } yield name

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

  property("moving doesn't turn") =
    forAll(validPosition, validDirection, validMoveCommand) { (myPosition: Position, myDirection: Direction, myMoveCommand: Command) =>

      val (actualPosition, actualDirection) = MarsRover.move((myPosition, myDirection), myMoveCommand)
      actualPosition != myPosition &&
        actualDirection == myDirection
    }
}