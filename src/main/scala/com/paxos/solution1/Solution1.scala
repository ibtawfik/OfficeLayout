package com.paxos.solution1
import com.paxos._

import scala.util.Try

/**
  * This is the original solution to the coffee machine problem that I presented during the interview. I've intentionally
  * over commented assuming the reader doesn't know scala well. This solution places the coffee machine in each space, then
  * does a dfs recursively to search the space for each employee. Time complexity here is n^2 * e where n is the number of
  * spaces in the office array and e is the number of employees.
  */
object Solution1 {

  def bestCoffeeMachineLocation(officeLayout: Array[Array[OfficeObject]], employeeLocations: List[(Int, Int)]): (Int, Int) = {
    var minDist = Int.MaxValue
    var bestLocation: (Int, Int) = null

    for (row <- officeLayout.indices) {
      for (column <- officeLayout(row).indices) {

        //If this is an empty spot then put a coffee machine in it
        if (officeLayout(row)(column) == Empty) {
          officeLayout(row).update(column, CoffeeMachine)

          //calculate the distance for each employee to the machine and sum
          val distance = employeeLocations.map(e => search(e, e, officeLayout, Set(), 0)).sum

          //If its shorter than the last location then update this location to the best
          if (distance < minDist) {
            minDist = distance
            bestLocation = (row, column)
          }

          //Reset the spot to empty for the next iteration of the loop
          officeLayout(row).update(column, Empty)
        }
      }
    }
    bestLocation
  }

  private def search(employeeLoc: (Int, Int), currentLocation: (Int, Int), officeLayout: Array[Array[OfficeObject]], visited: Set[(Int, Int)], distance: Int): Int = {
    //case we found the coffee machine return the accumulated distance
    if (!isOutOfBounds(currentLocation, officeLayout) && getObject(currentLocation, officeLayout) == CoffeeMachine) {
      //Scala always returns a value so we don't need to explicitly say return distance
      distance
    }

    //case the space has something other than the coffee  machine, like a column or another cube. OR we have gone out of bounds or we have already visited this path.
    else if (isOutOfBounds(currentLocation, officeLayout) || (getObject(currentLocation, officeLayout) != Empty && currentLocation != employeeLoc) || visited.contains(currentLocation)) {
      // then we want to say this is not a valid path by returning Int.MaxValue
      Int.MaxValue
    }

    //We haven't found the machine yet and we are on a valid path, then take the min of all the valid paths
    else {
      List(
        search(employeeLoc, (currentLocation._1 + 1, currentLocation._2), officeLayout, visited + currentLocation, distance + 1),
        search(employeeLoc, (currentLocation._1 - 1, currentLocation._2), officeLayout, visited + currentLocation, distance + 1),
        search(employeeLoc, (currentLocation._1, currentLocation._2 + 1), officeLayout, visited + currentLocation, distance + 1),
        search(employeeLoc, (currentLocation._1, currentLocation._2 - 1), officeLayout, visited + currentLocation, distance + 1)
      )
        .min
    }
  }

  private def getObject(location: (Int, Int), officeLayout: Array[Array[OfficeObject]]): OfficeObject =
    officeLayout(location._1)(location._2)

  private def isOutOfBounds(location: (Int, Int), officeLayout: Array[Array[OfficeObject]]): Boolean =
    Try(officeLayout(location._1)(location._2)).isFailure
}
