package com.paxos

/**
  * This is called a co-product in scala. It basically represents an OR between classes.
  */
trait OfficeObject
case object CoffeeMachine extends OfficeObject
case object Column extends OfficeObject
case object Employee extends OfficeObject
case object Empty extends OfficeObject