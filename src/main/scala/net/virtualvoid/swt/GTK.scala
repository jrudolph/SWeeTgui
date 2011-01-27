package net.virtualvoid.swt

import org.eclipse.swt
import swt.widgets.Tree

object GTK {
	def tightCells(t: Tree) {
		import swt.internal.gtk.OS
		val rendererMethod = t.getClass.getDeclaredMethod("getTextRenderer", java.lang.Long.TYPE)
		rendererMethod.setAccessible(true)
		val h = OS.gtk_tree_view_get_column (t.handle, 0)
		val r = rendererMethod.invoke(t,java.lang.Long.valueOf(h)).asInstanceOf[java.lang.Long].longValue
		OS.g_object_set (r, OS.ypad, 0, 0)
	}
}