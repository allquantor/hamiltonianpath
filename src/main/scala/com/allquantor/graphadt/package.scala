package com.allquantor

import com.allquantor.gridadt._


package object graphadt {

  sealed trait Graph[+E]

  case class Vertex[E](e: E) extends Graph[E]

  case class Edge[E](v1: Vertex[E], v2: Vertex[E]) extends Graph[E]

  /**
    *
    * This is the representation of a direct Graph, required for the further computation
    * of the given problem. Any operation do return a copy of the graph with the update
    * done by calling a certain operation. This is done to stay immutable.
    * However, the most elegant solution in my eyes would be to
    * implement the tree as a Zipper (https://wiki.haskell.org/Zipper). However, it would
    * be out of the scope for the given task.
    *
    * @param structure       (representing the connection between edges and vertices)
    * @param currentPosition current state of the graph (pointer)
    * @tparam E generic type param for vertex elements
    */

  case class DirectedGraph[E](structure: Map[Vertex[E], Iterable[Edge[E]]], currentPosition: Vertex[E]) extends Graph[E] {

    // O(1)
    def addVertex(v: Vertex[E]): DirectedGraph[E] = {
      this.copy(structure = structure + (v → structure.getOrElse(v, Nil)))
    }

    // O(1) - because of prepend
    def addEdge(e: Edge[E]): DirectedGraph[E] = {
      this.copy(structure =
        structure + (e.v1 → (Iterable(e) ++ structure.getOrElse(e.v1, Nil)))
      )
    }
  }

  case object DirectedGraph {

    private val lowestBoardNumber = 0


    // Initialize the assignment as graph with cells as vertices.
    // All Edges are valid move between two cells Position => Position.
    def apply(boardN:Int, startPosition: Vertex[Cell]): DirectedGraph[Cell] = {

      (lowestBoardNumber to boardN).foldLeft(new DirectedGraph[Cell](Map(), startPosition)) { (alphaGraph, xCoordinate) =>

        (lowestBoardNumber to boardN).foldLeft(alphaGraph) { (betaGraph, yCoordinate) =>

          // Create a vertex representing the coordinate (x,y)
          val currentViewedVertex = Vertex(Cell(visited = false, Position(xCoordinate, yCoordinate)))

          // Create all movement vertices and removes all that not satisfying the board constraints.
          val expandedPositions = expandPossibleMovements(currentViewedVertex.e.p).
            filter(moveConstraintSatisfied(_, boardN))

          // Add to currentViewedVertex all his neighbor vertices.
          expandedPositions.foldLeft(betaGraph) { (gammaGraph, position) =>
            val newEdge = Edge(currentViewedVertex, Vertex(Cell(visited = false, position)))
            gammaGraph.addEdge(newEdge)
          }
        }
      }
    }

  }

}
