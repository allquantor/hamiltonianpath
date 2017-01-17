package com.allquantor

import com.allquantor.alg.BruteforceHamilton

object PlatformAssignment2 extends App {


  /**
    *
    * Assignment:
    *
    * A pawn can move on 10x10 chequerboard horizontally, vertically and diagonally by these rules:
    * 1) 3 tiles moving North (N), West (W), South (S) and East (E)
    * 2) 2 tiles moving NE, SE, SW and NW
    * 3) Moves are only allowed if the ending tile exists on the board
    * 4) Starting from initial position, the pawn can visit each cell only once
    *
    * Write a program that finds out if it’s possible for the pawn to visit all tiles on the board
    * following the above rules, starting from any tile.
    *
    *
    *
    * Notes:
    *
    * The given problem can be seen as the Hamiltonian Path problem from Graph-Theory (https://en.wikipedia.org/wiki/Hamiltonian_path_problem)
    * Hence, the problem to find a HP is NP Complete - there is no polynomial time algorithm to find it.
    *
    * The following solution is a bruteforce + backtracking approach to find out the Hamiltonian Path.
    *
    */



  import graphadt._
  import gridadt._

  val boardSize = 9
  val startPosition = Vertex(Cell(visited = false, Position(0, 0)))


  val initialGraph = DirectedGraph(boardSize,startPosition)
  initialGraph


  BruteforceHamilton.deceide(initialGraph)

}
