/* ONLY
rewrite = ExplicitReturnTypes
 */
package test

import java.lang.{StringBuilder => JSB}

object ExplicitReturnTypes2 {
  implicit val sb = new StringBuilder
  implicit val jsb = new JSB
}
