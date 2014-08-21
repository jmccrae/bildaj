package bildaj

class BeanLikeBuilder[C <: Object](clazz : Class[C]) {
  private var fields = collection.mutable.Map[Int,Any]()

  protected def set(idx : Int, obj : Any) {
    fields(idx) = obj
  }

  def construct : C = {
    clazz.getConstructors() match {
      case Array(constructor) => {
        val params = for((paramaterType, i) <- constructor.getParameterTypes().zipWithIndex) yield {
          if(classOf[Set[_]].isAssignableFrom(paramaterType)) {
            fields.getOrElse(i, Set())
          } else {          
            fields.getOrElse(i, throw new RuntimeException("Argument %d is missing" format i))
          }
        }
        constructor.newInstance(params.map(_.asInstanceOf[Object]):_*).asInstanceOf[C]
      }
      case _ => throw new RuntimeException("Wrong number of constructors!")
    }
  }
}
