package test

package t1 {
  import scala.collection.immutable.{List => IList}

  object RenameUsed {
    IList.empty[String]
  }
}

package t2 {

  object RenameUnused
}

package t3 {
  import scala.util.{Success => _, _}

  object RenameUnusedWithUsedWildcard {
    Failure(new Exception())
  }
}

package t4 {

  object RenameUnusedWithUnusedWildcard
}

package t5 {

  object MultipleUnusedRenames
}

package t6 {
  import scala.concurrent.duration._

  object UnusedRenameFollowedByUsedWildcardInDifferentImports {
    Duration("10s")
  }
}

package t7 {
  import scala.io.StdIn._

  object UnusedRenameFollowingUsedWildcardInDifferentImports {
    lazy val l = readLine()
  }
}

package t8 {
  import scala.io._

  object UnusedRenameFollowedByUsedWildcardBetweenCommas {
    val c = Codec.UTF8
  }
}

package t9 {
  import java.util
  import util.concurrent
  import concurrent.atomic
  import atomic._

  object UnusedRenameWithUsedWildcardWhosePackagesAreImportedIndividually {
    new AtomicBoolean()
  }
}

package t10 {
  import java.{util => jutil}
  import jutil.{concurrent => jconcurrent}
  import jconcurrent.{locks => jlocks}
  import jlocks._

  object UnusedRenameWithUsedWildcardWhosePackagesAreImportedIndividuallyAndRenamed {
    new StampedLock()
  }
}

package t11 {

  object UnusedRenameFollowedByUsedWildcardInNestedScope {
    def foo: Int = { 1 + 1 }

    import java.time._
    Instant.now()
  }
}

package t12 {
  import java.util.function._

  object UsedWildcardFollowedByUnusedRenameInNestedScope {
    new Consumer[String] {
      override def accept(t: String): Unit = ???
    }
  }
}

package t13 {

  object UnusedRenameAndUsedWildcardInNonOverlappingScopes {
    def a: Unit = {
      import scala.collection.concurrent._
      TrieMap.empty[Int, Int]
    }

    def b: Unit = {
      println("b")
    }

    def c: Unit = {
      import scala.collection.concurrent._
      TrieMap.empty[Int, Int]
    }
  }
}

package t14 {

  class UnusedRenameInInnerScopesFollowedByUsedWildcardInOuterScope {
    def a: Unit = {
    }

    val b = {
      0
    }

    def this(x: Int) = {
      this()
    }

    {
    }

    trait Foo {
    }

    object L1 {
      object L2 {
        object L3 {
        }
      }
    }

    import scala.math._
    val r = random
  }
}
