package com.paxos

import com.paxos.solution1.Solution1
import com.paxos.solution2.Solution2


object Main {
  def main(args: Array[String]): Unit = {
    //A sample run to show how to call the functions and assert that we get the same solution
    val officeLayout: Array[Array[OfficeObject]] =
      Array(
        Array(Empty, Empty, Column, Empty, Employee),
        Array(Empty, Empty, Employee, Empty, Empty),
        Array(Empty, Empty, Empty, Empty, Empty),
        Array(Empty, Empty, Empty, Empty, Empty),
        Array(Employee, Employee, Empty, Empty, Empty)
      )

    val employeeLoc = List((0, 4), (4, 0), (4, 1))

    val solution1 = Solution1.bestCoffeeMachineLocation(officeLayout, employeeLoc)
    val solution2 = Solution2.bestCoffeeMachineLocation(officeLayout, employeeLoc)
    assert(solution1 == solution2)
    println(solution1._1 + ", " + solution1._2)
  }
}
