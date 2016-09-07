package com.filippodeluca.fp


class ValidatedApplicationSpec extends UnitSpec {

  import ValidatedApplication._

  "mkName" when {
    "name is null" should {
      "return invalid" in {
        mkName(null) shouldBe an[Invalid[_]]
      }
    }

    "name is empty" should {
      "return invalid" in {
        mkName("") shouldBe an[Invalid[_]]
      }
    }

    "name is a valid name" should {
      "return valid with the given nam" in {
        mkName("Filippo") shouldBe Valid(Name("Filippo"))
      }
    }
  }

  "mkAge" when {
    "age is less then 0" should {
      "return invalid" in {
        mkAge(-1) shouldBe an[Invalid[_]]
      }
    }

    "age is >= 0 y" should {
      "return valid with the given age" in {
        mkAge(10) shouldBe Valid(Age(10))
      }
    }
  }

  "mkPerson" when {
    "age is less than 0" should {
      "return invalid with age error" in {
        mkPerson("Filippo", -1) shouldBe Invalid(List("Age is out of range."))
      }
    }

    "name is null" should {
      "return invalid with name error" in {
        mkPerson(null, 10) shouldBe Invalid(List("Name is empty."))
      }
    }


    "name is empty" should {
      "return invalid with name error" in {
        mkPerson(null, 10) shouldBe Invalid(List("Name is empty."))
      }
    }

    "name is null and age is less than 0" should {
      "return invalid with name error and age error" in {
        mkPerson(null, -1) shouldBe Invalid(List("Name is empty.", "Age is out of range."))
      }
    }

    "name is empty and age is less than 0" should {
      "return invalid with name error and age error" in {
        mkPerson(null, -1) shouldBe Invalid(List("Name is empty.", "Age is out of range."))
      }
    }

    "name  and age are valid" should {
      "return a Person with the given name and age" in {
        mkPerson("Filippo", 35) shouldBe Valid(Person(Name("Filippo"), Age(35)))
      }
    }

  }
}
