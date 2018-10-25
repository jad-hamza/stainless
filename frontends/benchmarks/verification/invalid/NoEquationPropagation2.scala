import stainless.equations._

object NoEquationPropagation2 {
  def axiom(): Boolean = {
    ??? : Boolean
  } ensuring(_ => false)

  def f(x: BigInt, b: Boolean) = {
    x ==| b | // VC for checking b should fail even with `axiom()` below
    x ==| axiom() |
    x
  }
}
