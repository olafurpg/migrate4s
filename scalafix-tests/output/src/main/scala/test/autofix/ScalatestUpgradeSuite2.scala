package test.autofix

import scala.collection.mutable
import org.scalatest.matchers
import org.scalatest.matchers.should.Matchers

object ScalatestUpgradeSuite2 {
  object WithRename {
    import org.scalatest.matchers.should.{Matchers => ScalaTestMatchers}

    class UsesRename extends ScalaTestMatchers
    class UsesOriginal extends matchers.should.Matchers {
      val x = mutable.ListBuffer.empty[Int]
    }
  }
  object WithoutRename {
    class UsesRename extends Matchers
    class UsesOriginal extends matchers.should.Matchers
  }
}
