package com.filippodeluca.fp

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

abstract class UnitSpec
  extends WordSpec
  with Matchers
  with PropertyChecks
