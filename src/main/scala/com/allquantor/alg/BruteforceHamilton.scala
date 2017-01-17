package com.allquantor.alg


import com.allquantor.graphadt._
import com.allquantor.gridadt._


// Yes, I know that println is a side-effect. Don't like it either.

object BruteforceHamilton {

  private def expandAndRemove(g: DirectedGraph[Cell]): (
    Vertex[Cell],
      Iterable[Edge[Cell]],
      Map[Vertex[Cell], Iterable[Edge[Cell]]]
    ) = {

    println(s" -------------------------------------- ")
    println(s" Current Position  ${g.currentPosition}.")
    val currentPosition = g.currentPosition

    println(s" -------------------------------------- ")
    println(s" Removing all edges pointing to the current position.")
    val updatedStructure = g.structure.map { case (vertex, edgeList) =>
      val newEdgeList = edgeList.filter(e => e.v2 != currentPosition)
      (vertex, newEdgeList)
    }

    // expand possible steps
    val expandedSteps = updatedStructure.getOrElse(currentPosition, Nil)
    println(s" -------------------------------------- ")
    println(s" Expand possible steps of the current position: ${currentPosition} nextSteps:${expandedSteps.map(_.v2)} ")

    (currentPosition, expandedSteps, updatedStructure)
  }

  def deceide(g: DirectedGraph[Cell]): Boolean = {
    def tryOut(g: DirectedGraph[Cell]: Iterable[Option[DirectedGraph[Cell]]] = {

      val (currentPosition, expandedSteps, updatedStructure) = expandAndRemove(g)

      // check if this scope has visited all vertices
      val allNodesVisited = updatedStructure.keySet.forall(_.e.visited)

      (allNodesVisited, expandedSteps.isEmpty) match {
        case (true, _) =>
          println("*************************YEAHHHHHHHHHH WE DID IT!!!!*****************************************")
          Iterable(Some(g.copy(structure = updatedStructure)))

        case (false, true) =>
          println(s"*************************Reached the end of path without success for Graph:${updatedStructure.keys.foreach(println)}*******************")
          Iterable(None)
        case (false, _) =>
          // set the current position as visited
          val updatedCurrentPosition = Vertex(currentPosition.e.copy(visited = true))
          println(s" -------------------------------------- ")
          println(s" Update Current Position  ${g.currentPosition} as Visited ")
          val graphWithUpdatedPosition = g.copy(structure = updatedStructure - currentPosition + (updatedCurrentPosition -> expandedSteps))
          expandedSteps.flatMap(edge => tryOut(graphWithUpdatedPosition.copy(currentPosition = edge.v2)))
        case (_, _, _) =>
          throw new IllegalStateException(s"This state should never be reached. Something went badly wrong for Graph-state:${updatedStructure}")
      }
    }

    tryOut(g).exists(_.isDefined)
  }
}
