/*
rule = ScalatestUpgrade
 */
package test.autofix

import org.scalatest.Matchers._

object ScalatestUpgradeSuite {
  def foo(): Unit = 1 should be(1)
}
