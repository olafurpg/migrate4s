/* ONLY
rule = SAM
 */
package test

class SAM {

  val x = new Runnable {
    override def run(): Unit = println("RUN!!!!!")
  }

  def fun(p: WithParams) = ()

  fun(new WithParams() {
    def doit(a: Int, b: Int): Int = {
      // comments
      a + b
    }
  })

}
