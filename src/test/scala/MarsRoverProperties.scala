import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object MarsRoverProperties extends Properties("MarsRover") {
  val validPosition = for {
    n <- Gen.choose(-5, 5)
    m <- Gen.choose(-5, 5)
  } yield Position(n, m)

  val validDirection = Gen.oneOf(North, South, East, West)

  val validCommand = Gen.oneOf(Forward, Backward, Left, Right)

  val validTurnCommand = Gen.oneOf(Left, Right)

  val validMoveCommand = Gen.oneOf(Forward, Backward)

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

      val (actualPosition, _) = MarsRover.computeExactlyOneOfNewPositionOrNewDirection((myPosition, myDirection), myCommand)
      math.abs(actualPosition.x - myPosition.x) <= 1 &&
        math.abs(actualPosition.y - myPosition.y) <= 1
    }

  property("turning doesn't move") =
    forAll(validPosition, validDirection, validTurnCommand) { (myPosition: Position, myDirection: Direction, myTurnCommand: Command) =>

      val (actualPosition, actualDirection) = MarsRover.computeExactlyOneOfNewPositionOrNewDirection((myPosition, myDirection), myTurnCommand)
      actualPosition.x == myPosition.x &&
        actualPosition.y == myPosition.y &&
        actualDirection != myDirection
    }

  property("moving doesn't turn") =
    forAll(validPosition, validDirection, validMoveCommand) { (myPosition: Position, myDirection: Direction, myMoveCommand: Command) =>

      val (actualPosition, actualDirection) = MarsRover.computeExactlyOneOfNewPositionOrNewDirection((myPosition, myDirection), myMoveCommand)
      actualPosition != myPosition &&
        actualDirection == myDirection
    }

  property("(direction, command) moves to same position as (opposite direction, opposite command)") =
    forAll(validPosition, validDirection, validCommand) { (myPosition: Position, myDirection: Direction, myCommand: Command) =>

      val oppositeDirection = myDirection.opposite()
      val oppositeCommand = myCommand.opposite()

      val (actualPosition1, actualDirection1) = MarsRover.computeExactlyOneOfNewPositionOrNewDirection((myPosition, myDirection), myCommand)
      val (actualPosition2, actualDirection2) = MarsRover.computeExactlyOneOfNewPositionOrNewDirection((myPosition, oppositeDirection), oppositeCommand)

      actualPosition1 == actualPosition2
    }
  }