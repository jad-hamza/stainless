import stainless.equations._

object NoEquationPropagation4 {
  def axiom(): Boolean = {
    ??? : Boolean
  } ensuring(_ => false)

  def f(x: BigInt, y: BigInt, b: Boolean) = {
    x ==| true | // check for x = y should fail even though we verify y = x below
    y ==| axiom() |
    x
  }
}
