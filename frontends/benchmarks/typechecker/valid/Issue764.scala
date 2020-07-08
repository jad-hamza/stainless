import stainless.io.StdOut.println

object Issue764_1 {

  abstract class A
  abstract class B[X] {
    def p: Unit
  }
  abstract class C[X <: A, Y <:  C[X, Y]]  extends B[X]
  abstract class D [X  <: C[A, X]] {
    def m(x: X): Unit = {
      x.p
    }
  }
}

object Issue764_2 {
  abstract class C {
    def getC: C
  }
  abstract class B  extends C {}
  case class A[T <: B]() {
    def m(t:T): C = t.getC
  }
  def main(a: Array[String]): Unit = {
    case class BB() extends B {
      override def getC:C = this
      override def toString(): String = "<A BB>"
    }
    println(new A[BB].m(new BB))
  }
}

object Issue764_3 {
  abstract class B  /*extends C */ {
    def get: B
  }

  case class A[T <: B]() {
    def m(t:T): B = t.get
  }
}
