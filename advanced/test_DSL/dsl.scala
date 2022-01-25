object dsl {
implicit def set = Set

  def main(args:Array[String]) = {
    val bar = new Bar
    //set field “foo” of bar to “baz”
    println(bar.foo)
    Set.field("foo").of(bar).to("baz")
    println(bar.foo)
    set field "foo" of bar to "pino"
    println(bar.foo)
  }
}


class Bar(var foo:String = "Ciao") {}
object Set { def field(name:String) = { new Of(name)}}
class Of (name:String) { def of(obj:AnyRef) = {new To(name, obj)}}
class To (name:String, obj:AnyRef) {
  def to(value:String) = {
    val f = obj.getClass.getDeclaredField(name)
    f.setAccessible(true)
    f.set(obj, value)
  }
}
