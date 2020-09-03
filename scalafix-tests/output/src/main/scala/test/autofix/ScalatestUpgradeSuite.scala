package test.autofix

import org.scalatest.matchers.should.Matchers._

object ScalatestUpgradeSuite {
  def foo(): Unit = 1 should be(1)
}
