package scalafix.internal.v1

import scala.meta.internal.io.FileIO
import scala.meta.io.RelativePath
import scala.meta.Input
import scala.meta.AbsolutePath
import scala.meta.Source
import scala.meta.parsers.Parsed
import scalafix.internal.config.ScalafixConfig
import scalafix.internal.diff.DiffDisable
import scala.meta.internal.symtab.SymbolTable
import scalafix.internal.config.FilterMatcher
import scala.tools.nsc.interactive.Global
import scala.tools.nsc

case class ValidatedArgs(
    args: Args,
    symtab: SymbolTable,
    rules: Rules,
    config: ScalafixConfig,
    classLoader: ClassLoader,
    sourceroot: AbsolutePath,
    pathReplace: AbsolutePath => AbsolutePath,
    diffDisable: DiffDisable,
    callback: DelegatingMainCallback,
    semanticdbFileFilter: FilterMatcher,
    global: LazyValue[Option[Global]],
    checkCompileGlobal: LazyValue[Option[nsc.Global]]
) {
  def input(file: AbsolutePath): Input = {
    Input.VirtualFile(file.toString(), FileIO.slurp(file, args.charset))
  }

  def parse(input: Input): Parsed[Source] = {
    import scala.meta._
    val dialect = config.parser.dialectForFile(input.syntax)
    dialect(input).parse[Source]
  }

  def matches(path: RelativePath): Boolean =
    Args.baseMatcher.matches(path.toNIO) && {
      args.exclude.forall(!_.matches(path.toNIO))
    } && {
      // Respect -P:semanticdb:exclude and -P:semanticdb:include
      semanticdbFileFilter.eq(FilterMatcher.matchEverything) || {
        semanticdbFileFilter.matches(path.toString())
      }
    }

}
