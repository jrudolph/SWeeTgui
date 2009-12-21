package net.virtualvoid.swt

import Trees._
import _root_.net.virtualvoid.scala.Tools._

object Main extends TreeApp[AnyRef] {
	
	case class BoundField(o: AnyRef, field: java.lang.reflect.Field)
	def fieldsOf(o: AnyRef) = o.getClass.getFields.map(BoundField(o, _))
	case class BoundMethod0(o: AnyRef, method: java.lang.reflect.Method)
	def methodsOf(o: AnyRef) = o.getClass.getMethods.filter(m => m.getParameterTypes.length == 0 && m.getReturnType != classOf[Void]).map(BoundMethod0(o, _))
	
	def startObject = "Wurst"
	
	def treeCreator: ItemInfo[AnyRef] = 
	item[AnyRef].labelled(_.toString)
				 .|--(_.getClass)(item[Class[_]].labelled("Class: "+_).|--(treeCreator))
				 .|-*(fieldsOf _) { info =>
			 		info.labelled(_.field.getName)
			 			.|--(bf => bf.field.get(bf.o))(treeCreator)
			 	 }
			 	 .|-*(methodsOf _) { info =>
			 	 	info.labelled(_.method.getName)
			 	 		.|--(bm => bm.method.invoke(bm.o))(treeCreator)
			 	 }
	
	def main(args: Array[String]): Unit = run
}

