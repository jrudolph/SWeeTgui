package net.virtualvoid.swt.tree

import _root_.net.virtualvoid.scala.Tools._

object TreeExample extends TreeApp[AnyRef] {
	case class BoundField(o: AnyRef, field: java.lang.reflect.Field) {
    def value = field.get(o)
  }
	def fieldsOf(o: AnyRef) = o.getClass.getFields.map(BoundField(o, _))
	case class BoundMethod0(o: AnyRef, method: java.lang.reflect.Method) {
    def value = method.invoke(o)
  }
	def methodsOf(o: AnyRef) = o.getClass.getMethods.filter(m => m.getParameterTypes.length == 0 && m.getReturnType != classOf[Void]).map(BoundMethod0(o, _))
	
	def startObject = "Wurst"
	
	def treeCreator: ItemDesc[AnyRef] =
    node[AnyRef]
      .label(_.toString)
      .child(_.getClass) { //child =>
        node[Class[_]] decorateWith treeCreator label("Class: "+_)
      }
      .children(fieldsOf _)(
        node[BoundField]
          .child(_.value)(treeCreator)
          .label(_.field.getName)
      )
      .children(methodsOf _)(
        node[BoundMethod0]
          .child(_.value)(treeCreator)
          .label(_.method.getName)
      )
}