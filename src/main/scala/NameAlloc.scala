package HT
object NameAlloc {
  var counter: Int = 0
}

def AllocUniqueName(prefix: String): String = {
  NameAlloc.counter += 1
  prefix + "_" + NameAlloc.counter.toString
}