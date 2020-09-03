package scalafix.bsp

import ch.epfl.scala.bsp4j.BuildServer
import ch.epfl.scala.bsp4j.ScalaBuildServer

trait ScalafixBspServer extends BuildServer with ScalaBuildServer
