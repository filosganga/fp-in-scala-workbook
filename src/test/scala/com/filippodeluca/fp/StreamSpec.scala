package com.filippodeluca.fp

import scala.util.Random


class StreamSpec extends UnitSpec {

  val ones = Stream.constant(1)
  val randomNumbers = Stream.continuosly(() => Random.nextInt())

  "Stream.toList" when {
    "the stream is empty" should {
      "return List.nil" in {
        Stream.empty[Int].toList shouldBe List.nil[Int]
      }
    }

    "the stream contain one element" should {
      "return a List with the same element" in {
        Stream(1).toList shouldBe List(1)
      }
    }

    "the stream contain more elements" should {
      "return a List with the same elements in the same order" in {
        Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
      }
    }
  }

  "Stream.take" when {
    "The Stream is empty" should {
      "return an empty Stream" in {
        Stream.empty.take(10) shouldBe Stream.empty
      }
    }

    "The Stream is smaller than the requested size" should {
      "return the original Stream" in {
        Stream(1, 2, 3).take(10).toList shouldBe Stream(1, 2, 3).toList
      }
    }
  }

  "Stream.reverse" when {
    "The Stream is empty" should {
      "return an empty Stream" in {
        Stream.empty.reverse shouldBe Stream.empty
      }
    }

    "The Stream is non empty" should {
      "return a Stream with the same element in the reverse order" in {
        Stream(1, 2, 3).reverse.toList shouldBe Stream(3, 2, 1).toList
      }
    }
  }

  "Stream.drop" when {
    "the Stream is empty" should {
      "return empty" in {
        Stream.empty.drop(5) shouldBe Stream.empty
      }
    }
    "n is <= 0" should {
      "return the original Stream" in {
        Stream(1, 2, 3).drop(0).toList shouldBe Stream(1, 2, 3).toList
      }
    }
    "n is >= Stream.size" should {
      "return empty" in {
        Stream(1, 2, 3).drop(3) shouldBe Stream.empty
      }
    }
    "n < Stream.size" should {
      "return the Stream without dropped items" in {
        Stream(1, 2, 3, 4, 5).drop(2).toList shouldBe List(3, 4, 5)
      }
    }
  }

  "Stream.exists" when {
    "the Stream is empty" should {
      "return false" in {
        Stream.empty[Int].exists(_ == 5) shouldBe false
      }
    }

    "the predicate isn't satisfied for any items" should {
      "return false" in {
        Stream(1, 2, 3, 4, 6).exists(_ == 5) shouldBe false
      }
    }

    "the predicate is satisfied for some items" should {
      "return true" in {
        Stream(1, 2, 3, 4, 5).exists(_ == 3) shouldBe true
      }
    }
  }

  "Stream.forAll" when {
    "the Stream is empty" should {
      "return true" in {
        Stream.empty[Int].forAll(_ == 5) shouldBe true
      }
    }

    "the predicate isn't satisfied for all items" should {
      "return false" in {
        Stream(1, 2, 3, 4, 5).forAll(_ < 4) shouldBe false
      }
    }

    "the predicate is satisfied for all items" should {
      "return true" in {
        Stream(2, 4, 6, 8).forAll(_ < 10) shouldBe true
      }
    }
  }

  "Stream.takeWhile" when {
    "Stream is empty" should {
      "return empty" in {
        Stream.empty[Int].takeWhile(_ < 3) shouldBe Stream.empty
      }
    }

    "The first element does not satisfy the predicate" should {
      "return empty" in {
        Stream(3, 4, 5).takeWhile(_ < 3) shouldBe Stream.empty
      }
    }

    "The first n elements does satisfy the predicate" should {
      "return Stream of n elements" in {
        Stream(1, 2, 6, 3, 4).takeWhile(_ < 6).toList shouldBe List(1, 2)
      }
    }

    "All the elements does satisfy the predicate" should {
      "return the original Stream" in {
        Stream(1, 2, 6, 3, 4).takeWhile(_ < 60).toList shouldBe List(1, 2, 6, 3, 4)
      }
    }
  }

  "Stream.headOptionWithFoldRight" when {
    "the Stream is empty" should {
      "return None" in {
        Stream.empty.headOptionWithFoldRight shouldBe None
      }
    }

    "the Stream is non empty" should {
      "return the first element" in {
        Stream(1, 2, 3, 4).headOptionWithFoldRight shouldBe Some(1)
      }
    }
  }

  "Stream.filter" when {
    "Stream is empty" should {
      "return empty" in {
        Stream.empty[Int].filter(_ < 3) shouldBe Stream.empty
      }
    }

    "Some of the elements does satisfy the predicate" should {
      "return Stream of elements that satisfy the predicate" in {
        Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList shouldBe List(2, 4, 6)
      }
    }

    "All the elements does satisfy the predicate" should {
      "return the original Stream" in {
        Stream(2, 6, 4, 8).filter(_ % 2 == 0).toList shouldBe List(2, 6, 4, 8)
      }
    }
  }

  "Stream.append" when {
    "The Stream is empty" should {
      "return a Stream with only the appending element" in {
        Stream.empty.append(1).toList shouldBe List(1)
      }
    }

    "The Stream is not empty" when {
      "the appending Stream is empty" should {
        "return the original Stream" in {
          Stream(1, 2, 3).append(Stream.empty).toList shouldBe List(1, 2, 3)
        }
      }

      "the appending argument is an element" should {
        "return the original Stream with appended element" in {
          Stream(1, 2, 3).append(4).toList shouldBe List(1, 2, 3, 4)
        }
      }

      "the appending Stream is non empty" should {
        "return the original Stream with the appended stream" in {
          Stream(1, 2, 3).append(Stream(4, 5)).toList shouldBe List(1, 2, 3, 4, 5)
        }
      }
    }
  }

  "Stream.flatMap" when {
    "The Stream is empty" should {
      "return an empty Stream" in {
        Stream.empty[Int].flatMap(x => Stream(x * 2)) shouldBe Stream.empty
      }
    }

    "The Stream is not empty" when {
      "the map function return an empty Stream" should {
        "return an empty Stream" in {
          Stream(1, 2, 3).flatMap(_ => Stream.empty) shouldBe Stream.empty
        }
      }

      "the map function return an non empty Stream" should {
        "return the concatenation of mapped Stream" in {
          Stream(1, 2, 3).flatMap(x => Stream(x, -x)).toList shouldBe List(1, -1, 2, -2, 3, -3)
        }
      }
    }
  }

  "Stream function" should {
    "be composed" in {
      Stream(1, 2, 3, 4)
        .map { x =>
          x + 10
        }
        .filter { x =>
          x % 2 == 0
        }
    }
  }

  "Stream" should {

    "be lazy in map" in {
      ones.map(_ + 1).take(5).toList shouldBe List(2,2,2,2,2)
    }

    "be lazy in exists" in {
      ones.exists(_ == 1) shouldBe true
    }

    "be lazy in takeWhile" in {
      ones.takeWhile(_ == 1).exists(_ == 1) shouldBe true
    }

    "be lazy in forall" in {
      ones.forAll(_ != 1) shouldBe false
    }

    "be lazy in filter" in {
      randomNumbers.filter(_ < 10).take(100) should have size 100
    }
  }

  "Stream.constants" should {
    "return always the same number" in {
      Stream.constant(1).take(1000).forAll(_ == 1) shouldBe true
    }
  }

  "Stream.fibs" should {
    "return fibonacci numbers" in {
      Stream.fibs.take(8).toList shouldBe List(0,1,1,2,3,5,8,13)
    }
  }


  "Stream.takeViaUnfold" when {
    "the Stream is empty" should {
      "return empty" in {
        Stream.empty.takeViaUnfold(10) shouldBe Stream.empty
      }
    }

    "n is 0" should {
      "return empty" in {
        Stream(1,2,3).takeViaUnfold(0) shouldBe Stream.empty
      }
    }

    "the Stream is not empty and n >0 and n < Stream.size" should {
      "return a Stream of size n" in {
        Stream(1,2,3, 4, 5).takeViaUnfold(3).toList shouldBe List(1,2,3)
      }
    }
  }

  "Stream.zipWith" when {
    "The Stream is empty" should {
      "return empty" in {
        Stream.empty[Int].zipWith(Stream(1,2,3))(_ + _) shouldBe Stream.empty
      }
    }

    "The zipping Stream is empty" should {
      "return empty" in {
        Stream(1,2,3).zipWith(Stream.empty[Int])(_ + _) shouldBe Stream.empty
      }
    }

    "Both stream are not empty" should {
      "return a string of the smaller size" in {
        Stream(1,2,3).zipWith(Stream(1,2))(_ + _).size shouldBe 2
      }
      "return a combined Stream" in {
        Stream(1,2,3).zipWith(Stream.constant(2))(_ * _).toList shouldBe List(2,4,6)
      }
    }
  }

  "Stream.zipAll" when {
    "both Streams are empty" should {
      "return empty" in {
        Stream.empty.zipAll(Stream.empty) shouldBe Stream.empty
      }
    }

    "one Streams is empty" should {
      "return a Stream with size equal to the left non-empty Stream" in {
        Stream.empty.zipAll(Stream(1,2,3)).size shouldBe 3
      }
      "return a Stream with size equal to the right non-empty Stream" in {
        Stream(1,2,3).zipAll(Stream.empty).size shouldBe 3
      }
    }

    "both Streams are non-empty" should {
      "return a Stream with size equal to the bigger Stream" in {
        Stream(1,2,3).zipAll(Stream(1,2,3,4)).size shouldBe 4
      }
      "return a Stream with all the zipped entity and few Nones" in {
        Stream(1).zipAll(Stream('a','b')).toList shouldBe List(Some(1)->Some('a'), None->Some('b'))
      }
    }
  }
}
