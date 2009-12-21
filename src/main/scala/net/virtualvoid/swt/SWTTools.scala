package net.virtualvoid.swt

object SWTTools {
	import _root_.org.eclipse.swt
	import swt.SWT
	import swt.widgets.{Widget, Display, MessageBox, Shell}
	
	case class ItemStorage[T](label: String){
		def apply(w: Widget): Option[T] = Option(w.getData(label).asInstanceOf[T])
		def getOr(w: Widget)(default: => T) = apply(w) match {
			case Some(x) => x
			case None => val x = default; update(w, x); x
		}
		def update(w: Widget, data: T): Unit = w.setData(label, data)
		def clear(w: Widget): Unit = w.setData(label, null)
	}
	trait WithStorage {
		def apply[T](store: ItemStorage[T]): Option[T]
		def update[T](store: ItemStorage[T], data: T): Unit
	}
	implicit def widgetIsStorage(w: Widget): WithStorage = new WithStorage {
		def apply[T](store: ItemStorage[T]): Option[T] = store(w)
		def update[T](store: ItemStorage[T], data: T): Unit = store(w) = data
	}
	
	def infoBox(str: String): Unit = {
		val box = new MessageBox(Display.getDefault.getActiveShell, SWT.ICON_INFORMATION)
		box.setMessage(str)
		box.open
	}
	
	def textBox(str: String): Unit = {
		import swt.layout.FillLayout
		import swt.widgets.Text
		import swt.graphics.Font
		
		val box = new Shell(Display.getCurrent, SWT.DIALOG_TRIM | SWT.RESIZE);
		box.setLayout(new FillLayout)
		
		val edit = new Text(box, SWT.MULTI | SWT.V_SCROLL)
		edit.setText(str)
		edit.setEditable(false)
		edit.setFont(new Font(Display.getCurrent,"mono" ,8 ,SWT.NORMAL))
		
		box.open
	}
}
