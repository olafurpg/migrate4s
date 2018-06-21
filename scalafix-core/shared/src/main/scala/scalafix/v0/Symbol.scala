package scalafix.v0

import scala.compat.Platform.EOL
import scala.meta.internal.semanticdb3.Scala._

sealed trait Symbol extends Product {
  def syntax: String
  def structure: String
}

object Symbol {
  case object None extends Symbol {
    override def toString = syntax
    override def syntax = Symbols.None
    override def structure = s"""Symbol.None"""
  }

  final case class Local(id: String) extends Symbol {
    override def toString = syntax
    override def syntax = Symbols.Local(id)
    override def structure = s"""Symbol.Local("$id")"""
  }

  final case class Global(owner: Symbol, signature: Signature) extends Symbol {
    override def toString = syntax
    override def syntax = Symbols.Global(owner.syntax, signature.syntax)
    override def structure = s"""Symbol.Global(${owner.structure}, ${signature.structure})"""
  }

  final case class Multi(symbols: List[Symbol]) extends Symbol {
    override def toString = syntax
    override def syntax = symbols.mkString(";")
    override def structure = s"""Symbol.Multi(${symbols.map(_.structure).mkString(", ")})"""
  }

  // NOTE: The implementation is really tedious, and something like Fastparse
  // would definitely make it more palatable. However, as we have benchmarked,
  // that will also significantly slow down parsing. For more details, check out:
  // https://github.com/scalameta/scalameta/pull/1241.
  def apply(s: String): Symbol = {
    object naiveParser {
      var i = 0
      def fail(message: String = "invalid symbol format") = {
        val caret = " " * (i - 1) + "^"
        throw new IllegalArgumentException(s"$message$EOL$s$EOL$caret")
      }

      val BOF = '\u0000'
      val EOF = '\u001A'
      var currChar = BOF
      def readChar(): Char = {
        if (i >= s.length) {
          if (i == s.length) {
            currChar = EOF
            i += 1
            currChar
          } else {
            fail()
          }
        } else {
          currChar = s(i)
          i += 1
          currChar
        }
      }

      def parseName(): String = {
        val buf = new StringBuilder()
        if (currChar == '`') {
          while (readChar() != '`') buf += currChar
          readChar()
        } else {
          if (!Character.isJavaIdentifierStart(currChar)) fail()
          buf += currChar
          while (Character.isJavaIdentifierPart(readChar())) {
            if (currChar == EOF) return buf.toString
            buf += currChar
          }
        }
        buf.toString
      }

      def parseSymbol(owner: Symbol): Symbol = {
        def global(signature: Signature): Symbol.Global = {
          if (owner == Symbol.None && signature != Signature.Term("_root_")) {
            val root = Symbol.Global(Symbol.None, Signature.Term("_root_"))
            Symbol.Global(root, signature)
          } else {
            Symbol.Global(owner, signature)
          }
        }
        def local(name: String): Symbol.Local = {
          Symbol.Local(name)
        }
        if (currChar == EOF) {
          owner
        } else if (currChar == ';') {
          fail("multi symbols are not supported")
        } else if (currChar == '[') {
          readChar()
          val name = parseName()
          if (currChar != ']') fail()
          else readChar()
          parseSymbol(global(Signature.TypeParameter(name)))
        } else if (currChar == '(') {
          readChar()
          val name = parseName()
          if (currChar != ')') fail()
          else readChar()
          parseSymbol(global(Signature.TermParameter(name)))
        } else {
          val name = parseName()
          if (currChar == '#') {
            readChar()
            parseSymbol(global(Signature.Type(name)))
          } else if (currChar == '.') {
            readChar()
            parseSymbol(global(Signature.Term(name)))
          } else if (currChar == '(') {
            val start = i - 1
            while (readChar() != ')') {
              if (currChar == '`') {
                while (readChar() != '`') {}
              }
            }
            readChar()
            if (currChar != '.') fail()
            else readChar()
            val disambiguator = s.substring(start, i - 2)
            parseSymbol(global(Signature.Method(name, disambiguator)))
          } else {
            if (owner == Symbol.None && name.startsWith("local")) local(name)
            else fail()
          }
        }
      }

      def entryPoint(): Symbol = {
        readChar()
        parseSymbol(Symbol.None)
      }
    }
    naiveParser.entryPoint()
  }
  def unapply(sym: String): Option[Symbol] = scala.util.Try(apply(sym)).toOption
}





