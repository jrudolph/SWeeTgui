package net.virtualvoid.swt

import _root_.org.eclipse.swt
import swt.SWT
import swt.widgets.{Shell, Tree}
import swt.layout.FillLayout
import Trees._
import Tools._

object Main {
	
	case class BoundField(o: AnyRef, field: java.lang.reflect.Field)
	def fieldsOf(o: AnyRef) = o.getClass.getFields.map(BoundField(o, _))
	case class BoundMethod0(o: AnyRef, method: java.lang.reflect.Method)
	def methodsOf(o: AnyRef) = o.getClass.getMethods.filter(m => m.getParameterTypes.length == 0 && m.getReturnType != classOf[Void]).map(BoundMethod0(o, _))
	
	def main(args: Array[String]){
		val shell = new Shell
		shell.setLayout(new FillLayout)
		val tree = new Tree(shell, SWT.NONE)
		tree.addTreeListener(TreeExpansionListener)
		
		def objInfo: ItemInfo[AnyRef] = 
			item[AnyRef].labelled(_.toString)
						 .|--(_.getClass)(item[Class[_]].labelled("Class: "+_).|--(objInfo))
						 .|-*(fieldsOf _) { info =>
					 		info.labelled(_.field.getName)
					 			.|--(bf => bf.field.get(bf.o))(objInfo)
					 	 }
					 	 .|-*(methodsOf _) { info =>
					 	 	info.labelled(_.method.getName)
					 	 		.|--(bm => bm.method.invoke(bm.o))(objInfo)
					 	 }
		
		objInfo.create(tree, "Wurst")

		shell.open
		val display = shell.getDisplay
		while (!shell.isDisposed) {
			if (!display.readAndDispatch) display.sleep
		}
	}
}
