package playground

val up  = "u"
val down = "d"
val right = "r"
val left = "l"
case class Game(m: Int, n: Int, snake: Snake) extends SnakeGame {

  val food = Food(getAppleNextLocation())

  override def changeDirection(direction: Direction): Unit = direction.direction match {
    case up => Snake(head = snake.head, elements = snake.elements, lastGeneralDirection = up)
    case down => Snake(head = snake.head, elements = snake.elements, lastGeneralDirection = down)
    case left => Snake(head = snake.head, elements = snake.elements, lastGeneralDirection = left)
    case right => Snake(head = snake.head, elements = snake.elements, lastGeneralDirection = right)
    case _   => Snake(head = snake.head, elements = snake.elements, lastGeneralDirection = snake.lastGeneralDirection)
  }

  override def moveOneStep() = {
    val (headLocationX, headLocationY) = (snake.head.x, snake.head.y)
    val lastGeneralDirection           = Direction(snake.lastGeneralDirection)
    val nextLocation                 = getNextPosition(headLocationX, headLocationY , lastGeneralDirection)

    isGameOver(nextLocation)

    if (nextLocation.x == food.location.x && nextLocation.y == food.location.y) {
      Snake(head = Location(food.location.x, food.location.y), elements = snake.elements, lastGeneralDirection = snake.lastGeneralDirection)
      // modify free locations (part 2 of the question)
    }

  }

  override def isGameOver(nextLocation: Location) = {
    checkSnakeOutOfBounds(m,n, nextLocation)
    checkIfSelfEaten(nextLocation, snake.elements)
  }

  def checkIfSelfEaten(nextLocation: Location, elements: Map[Location, Location]) =
    if (elements.contains(nextLocation))
      throw new RuntimeException("Game Over! snake has eaten itself!")

  def checkSnakeOutOfBounds(m: Int, n: Int, nextLocation: Location) =
    if (nextLocation.x < 0 || nextLocation.x > m || nextLocation.y < 0 || nextLocation.y > n)
      throw new RuntimeException("Game Over! snake is out of bounds!")
}

case class Snake(head: Location, elements: Map[Location, Location], lastGeneralDirection: String) {}

trait SnakeGame {
  def changeDirection(direction: Direction)

  def moveOneStep()

  def isGameOver(nextLocation: Location)

  def getNextPosition(x: Int, y: Int, direction: Direction): Location = ???

  def getAppleNextLocation(): Location = ???

}

case class Direction(direction: String)
case class Location(x: Int, y: Int)
case class Food(location: Location)


/*
No time limit
Performance: runtime beats memory

1. changeDirection(Direction): // updates the snake's general direction

2. moveOneStep():
  // assume it's being called every X ms
  // moves the snake one step forward

3. isGameOver(??) -> ??

Utility methods:
  1. getNextPosition(x, y, direction)
  2. getAppleNextLocation()

 */

