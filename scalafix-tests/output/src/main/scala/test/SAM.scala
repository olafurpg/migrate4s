package test

class SAM {

  val x: Runnable = () => println("RUN!!!!!")

  def fun(p: WithParams) = ()

  fun((a, b) => {
      // comments
      a + b
    })

}
