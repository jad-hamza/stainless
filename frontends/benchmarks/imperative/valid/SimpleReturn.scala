object SimpleReturn {
  def example1: Int = {
    return 0
    1
  }

  def example2(x: Int): Int = {
    val a = if (x > 0) { return 1 } else { 2 }
    3
  }

  def tests() = {
    assert(example1 == 0)
    assert(example2(10) == 1)
    assert(example2(-10) == 3)
  }
}
