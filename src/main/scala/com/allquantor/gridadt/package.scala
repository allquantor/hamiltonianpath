package com.allquantor

package object gridadt {

  /**
    *
    * Data-structure to natively represent the Grid inside the Graph
    * Hence, the rules for the movement are given on a Grid example.
    *
    */

  sealed trait Grid

  case class Position(x: Int, y: Int) extends Grid

  case class Cell(visited: Boolean, p: Position) extends Grid


  /**
    * A constraint representing the movement restrictions given by board.
    * Constraint is valid for boards sqrt(pow(n,2)) == n
    *
    * boardSize should be given by consideration that the smallest number in a board is 0
    *
    *
    */
  val moveConstraintSatisfied: (Position, Int) => Boolean =
    (p: Position, boardSize: Int) => p.x >= 0 && p.x <= boardSize && p.y <= boardSize && p.y >= 0

  /**
    * According to the assignment constraints there are at most 8 possible movements for a cell.
    */
  val expandPossibleMovements: Position => Iterable[Position] = (p: Position) =>
    Iterable(
      Position(p.x + 3, p.y),
      Position(p.x - 3, p.y),
      Position(p.x, p.y + 3),
      Position(p.x, p.y - 3),
      Position(p.x + 2, p.y + 2),
      Position(p.x - 2, p.y - 2),
      Position(p.x + 2, p.y - 2),
      Position(p.x - 2, p.y + 2)
    )




}
