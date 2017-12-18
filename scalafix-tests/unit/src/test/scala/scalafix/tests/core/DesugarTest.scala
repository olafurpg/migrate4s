package scalafix.tests.core

import org.scalameta.logger

class DesugarTest extends BaseSemanticTest("DesugarTest") {
  test("desugar") {
    _db = _db.withDocuments(_doc :: Nil)
    val desugared = index.desugar(source)
    logger.elem(desugared)
  }
}
