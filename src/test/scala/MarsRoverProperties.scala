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

  val validCommand = for {
    name <- Gen.oneOf("f", "b", "l", "r")
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

    val myTuple = (myPosition, myDirection)
    val result = MarsRover.move(myTuple, myCommand)
    math.abs(result._1.x - myTuple._1.x) <= 1 &&
      math.abs(result._1.y - myTuple._1.y) <= 1
  }
}
