/*
rule = RemoveUnused
 */
package test

package t1 {
  import scala.collection.immutable.{List => IList}

  object RenameUsed {
    IList.empty[String]
  }
}

package t2 {
  import scala.collection.immutable.{Set => ISet}

  object RenameUnused
}

package t3 {
  import scala.util.{Success => UnusedSuccess, _}

  object RenameUnusedWithUsedWildcard {
    Failure(new Exception())
  }
}

package t4 {
  import scala.concurrent.{Future => UnusedSuccess, _}

  object RenameUnusedWithUnusedWildcard
}

package t5 {
  import scala.collection.mutable.{Map => MutableMap, HashMap => MutableHashMap, HashSet => MutableHashSet}
  import scala.collection.mutable.{Map => MMap, HashMap => MHashMap, HashSet => MHashSet}

  object MultipleUnusedRenames
}

package t6 {
  import scala.concurrent.duration.{FiniteDuration => FDuration}
  import scala.concurrent.duration._

  object UnusedRenameFollowedByUsedWildcardInDifferentImports {
    Duration("10s")
  }
}

package t7 {
  import scala.io.StdIn._
  import scala.io.StdIn.{readByte => readL}

  object UnusedRenameFollowingUsedWildcardInDifferentImports {
    lazy val l = readLine()
  }
}

package t8 {
  import scala.io.{Source => S}, scala.io._

  object UnusedRenameFollowedByUsedWildcardBetweenCommas {
    val c = Codec.UTF8
  }
}

package t9 {
  import java.util.concurrent.atomic.{AtomicInteger => A}
  import java.util
  import util.concurrent
  import concurrent.atomic
  import atomic._

  object UnusedRenameWithUsedWildcardWhosePackagesAreImportedIndividually {
    new AtomicBoolean()
  }
}

package t10 {
  import java.util.concurrent.locks.{ReentrantLock => RL}
  import java.{util => jutil}
  import jutil.{concurrent => jconcurrent}
  import jconcurrent.{locks => jlocks}
  import jlocks._

  object UnusedRenameWithUsedWildcardWhosePackagesAreImportedIndividuallyAndRenamed {
    new StampedLock()
  }
}

package t11 {
  import java.time.{Clock => jClock}

  object UnusedRenameFollowedByUsedWildcardInNestedScope {
    def foo: Int = { 1 + 1 }

    import java.time._
    Instant.now()
  }
}

package t12 {
  import java.util.function._

  object UsedWildcardFollowedByUnusedRenameInNestedScope {
    import java.util.function.{Predicate => Pred}
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
      import scala.collection.concurrent.{Map => CMap}
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
      import scala.math.{BigDecimal => BigDec}
    }

    val b = {
      import scala.math.{BigInt => BigInteger}
      0
    }

    def this(x: Int) = {
      this()
      import scala.math.{E => e}
    }

    {
      import scala.math.{random => randomize}
    }

    trait Foo {
      import scala.math.{Pi => PI}
    }

    object L1 {
      import scala.math.{Pi => PI}
      object L2 {
        import scala.math.{Pi => PI}
        object L3 {
          import scala.math.{Pi => PI}
        }
      }
    }

    import scala.math._
    val r = random
  }
}

