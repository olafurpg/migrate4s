/*
rule = ScalatestUpgrade
 */
package test.autofix

import scala.collection.mutable

object ScalatestUpgradeSuite2 {
  object WithRename {
    import org.scalatest.{Matchers => ScalaTestMatchers}

    class UsesRename extends ScalaTestMatchers
    class UsesOriginal extends org.scalatest.Matchers {
      val x = mutable.ListBuffer.empty[Int]
    }
  }
  object WithoutRename {
    import org.scalatest.Matchers
    class UsesRename extends Matchers
    class UsesOriginal extends org.scalatest.Matchers
  }
}
