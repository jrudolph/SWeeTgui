package net.virtualvoid.swt

object SWTTools {
	import _root_.org.eclipse.swt.widgets.Widget
	case class ItemStorage[T](label: String){
		def apply(w: Widget): Option[T] = Option(w.getData(label).asInstanceOf[T])
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
}
