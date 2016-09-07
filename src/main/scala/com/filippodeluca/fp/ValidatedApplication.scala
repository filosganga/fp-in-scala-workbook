package com.filippodeluca.fp


/**
  * Ex. 4.8 We have changed only the Validated implementation, this code is exacly the same as the EitherApplication
  * but it is able to return all errors because the map2 implementation of Validated accumulate the errors.
  */
object ValidatedApplication {

  import Validated._

  final case class Person(name: Name, age: Age)
  final case class Name(value: String) extends AnyVal
  final case class Age(value: Int) extends AnyVal

  def mkName(name: String): Validated[String, Name] =
    if (name == "" || name == null )
      invalid(List("Name is empty."))
    else
      valid(new Name(name))

  def mkAge(age: Int): Validated[String, Age] =
    if (age < 0)
      invalid(List("Age is out of range."))
    else
      valid( new Age(age))

  def mkPerson(name: String, age: Int): Validated[String, Person] =
    mkName(name).map2(mkAge(age))(Person.apply)

}
