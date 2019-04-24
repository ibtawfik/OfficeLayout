package com.paxos.solution2

import com.paxos.{Empty, OfficeObject}

import scala.util.Try

/**
  * This is a better solution to the problem that we ran out of time for.
  * It also uses a dfs to search the space for each employee. But rather than placing the coffee machine in every open
  * space and testing the distances we instead find the min distance to every spot for every employee and sum those arrays
  * to get a total distance array. We then return the index for the space in the array with the smallest total distance.
  * This solution reduces the runtime complexity to n * e where n is the number of spaces in the office array and e is
  * the number of employees.
  */
object Solution2 {

  def bestCoffeeMachineLocation(officeLayout: Array[Array[OfficeObject]], employeeLocations: List[(Int, Int)]): (Int, Int) = {
    val totalDistArray: Array[Array[Int]] =
      employeeLocations
        .map(e => getDistances(e, officeLayout)) //generate a distance map for each employee
        .reduce(combine) //Add all the distances together

    var minDist = Int.MaxValue
    var bestLocation: (Int, Int) = null

    for (row <- totalDistArray.indices) {
      for (column <- totalDistArray(row).indices) {
        //Get the aggregate distance of each position and track the min
        val distance = totalDistArray(row)(column)
        if (distance < minDist) {
          minDist = distance
          bestLocation = (row, column)
        }
      }
    }
    bestLocation
  }


  private def getDistances(employeeLoc: (Int, Int), officeLayout: Array[Array[OfficeObject]]): Array[Array[Int]] = {
    val distArray: Array[Array[Int]] = infiniteArray(officeLayout)

    //This is a nested function in scala. It's a convenient way of defining a recursive function that behaves like a loop
    //to update some shared state from the outer level of the function
    def updateDistance(currentLocation: (Int, Int), distance: Int): Unit = {

      //Continue to explore the space if the space is empty and the new distance found is less than any prior distance found
      if (!isOutOfBounds(currentLocation, officeLayout) &&
        (getValue(currentLocation, officeLayout) == Empty && distance < getValue(currentLocation, distArray)) || currentLocation == employeeLoc) {

        //if we are not at an initial location then update the distance
        if (employeeLoc != currentLocation) distArray(currentLocation._1)(currentLocation._2) = distance

        //Move through the rest of the space and update the distance array
        updateDistance((currentLocation._1 + 1, currentLocation._2), distance + 1)
        updateDistance((currentLocation._1 - 1, currentLocation._2), distance + 1)
        updateDistance((currentLocation._1, currentLocation._2 + 1), distance + 1)
        updateDistance((currentLocation._1, currentLocation._2 - 1), distance + 1)
      }

    }
    //Call the nested function and return the updated distance array
    updateDistance(employeeLoc, 0)
    distArray
  }

  private def infiniteArray(officeLayout: Array[Array[OfficeObject]]): Array[Array[Int]] =
    officeLayout.map(a => a.map(_ => Int.MaxValue))

  private def getValue[T](location: (Int, Int), officeLayout: Array[Array[T]]): T =
    officeLayout(location._1)(location._2)

  private def isOutOfBounds(location: (Int, Int), officeLayout: Array[Array[OfficeObject]]): Boolean =
    Try(officeLayout(location._1)(location._2)).isFailure

  private def combine(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    val combinedArray = new Array[Array[Int]](a.length)
    for (row <- a.indices) {
      combinedArray.update(row, new Array[Int](a(row).length))
      for (column <- a(row).indices) {
        combinedArray(row)(column) = a(row)(column) + b(row)(column)
      }
    }
    combinedArray
  }

}
