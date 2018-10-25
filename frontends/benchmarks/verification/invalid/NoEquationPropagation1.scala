import stainless.equations._

object NoEquationPropagation1 {
  def axiom(): Boolean = {
    ??? : Boolean
  } ensuring(_ => false)

  def f(x: BigInt, b: Boolean) = {
    x ==| axiom() |
    x ==| b | // VC for checking b should fail even with `axiom()` above
    x
  }
}
