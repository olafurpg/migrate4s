package scalafix

import scala.meta.Tree
import scala.meta.parsers.Parse
import scalafix.rewrite.ScalafixRewrite

package object config extends ScalafixMetaconfigReaders {
  type MetaParser = Parse[_ <: Tree]

  type ConfigAndRewrite = (ScalafixConfig, List[ScalafixRewrite])
}
