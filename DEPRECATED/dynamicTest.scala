import scala.language.dynamics

class Parent extends Dynamic:
    var map = Map.empty[String, Any]
    def selectDynamic(name: String) =
        map get name getOrElse sys.error("method not found")
    def updateDynamic(name: String)(value: Any) =
        map += name -> value
    def applyDynamic(name: String)(args: Any*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    def applyDynamicNamed(name: String)(args: (String, Any)*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    var x = 0
    def sayHi = print("hi")

class Child extends Parent:
    this.y = 0
    this.sayHello = print("hello")