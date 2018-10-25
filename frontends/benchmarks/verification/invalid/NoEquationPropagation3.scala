import stainless.equations._

object NoEquationPropagation3 {
  def axiom(): Boolean = {
    ??? : Boolean
  } ensuring(_ => false)

  def f(x: BigInt, y: BigInt, b: Boolean) = {
    x ==| axiom() |
    y ==| true | // check for y = x should fail even though we verify x = y above
    x
  }
}
