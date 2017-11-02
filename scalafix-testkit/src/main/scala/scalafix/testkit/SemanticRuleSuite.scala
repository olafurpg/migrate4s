package scalafix
package testkit

import scala.meta._
import scalafix.internal.util.EagerInMemorySemanticdbIndex
@deprecated("Moved to scalafix.testkit.scalatest.SemanticRuleSuite", "0.5.4")
abstract class SemanticRuleSuite(
    val index: SemanticdbIndex,
    val expectedOutputSourceroot: Seq[AbsolutePath]
) extends BaseSemanticRuleSuite
    with BaseScalafixSuite {
  def this(
      index: SemanticdbIndex,
      inputSourceroot: AbsolutePath,
      expectedOutputSourceroot: Seq[AbsolutePath]
  ) = this(
    index,
    expectedOutputSourceroot
  )
  def this(
      database: Database,
      inputSourceroot: AbsolutePath,
      expectedOutputSourceroot: Seq[AbsolutePath]
  ) =
    this(
      EagerInMemorySemanticdbIndex(
        database,
        Sourcepath(inputSourceroot),
        Classpath(Nil)),
      inputSourceroot,
      expectedOutputSourceroot
    )
}
