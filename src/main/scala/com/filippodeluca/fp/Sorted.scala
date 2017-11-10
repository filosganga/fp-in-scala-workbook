package com.filippodeluca.fp

trait Sorted {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    def loop(index: Int, orderedSoFar: Boolean): Boolean = if(orderedSoFar && index < as.length - 1) {
      loop(index + 1, ordered(as(index), as(index + 1)))
    } else {
      orderedSoFar
    }

    loop(0, orderedSoFar = true)
  }

}
