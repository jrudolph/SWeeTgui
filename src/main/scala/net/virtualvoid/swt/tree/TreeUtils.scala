package net.virtualvoid.swt
package tree

import SWTTools._
import net.virtualvoid.scala.Tools._
import _root_.org.eclipse.swt
import swt.SWT
import swt.widgets.{ Tree, TreeItem, Menu, MenuItem }
import swt.events.{ TreeEvent, TreeListener, MouseEvent, MouseAdapter, SelectionAdapter, SelectionEvent }

object TreeUtils {
  type ItemParent = Either[Tree, TreeItem]
	/** Account for the missing common ancestor by using Either */
	def createTreeItem(parent: ItemParent): TreeItem = parent match {
		case Left(t) => new TreeItem(t, SWT.NONE)
		case Right(item) => new TreeItem(item, SWT.NONE)
	}
	val childrenGenerator = ItemStorage[(TreeItem, () => Unit)]("children")
	val onDblClickHandler = ItemStorage[TreeItem => Unit]("dblClickHandler")
	val ctxMenu = ItemStorage[Menu]("ctxMenu")

	def registerGenerator(it: TreeItem)(f: => Unit) {
		it(childrenGenerator) match {
			case Some((dummy, gen)) => it(childrenGenerator) = (dummy, { () => gen();f;() })
			case None => it(childrenGenerator) = (new TreeItem(it, SWT.NONE) ~! (_.setText("<dummy>")), () => f)
		}
	}

	object TreeListener extends MouseAdapter with TreeListener {
			override def treeCollapsed(e: TreeEvent): Unit = {}
			override def treeExpanded(e: TreeEvent): Unit =
				for ((dummy, childrenGen) <- e.item(childrenGenerator)) {
					childrenGenerator.clear(e.item)
					childrenGen()
					dummy.dispose
				}

			override def mouseDoubleClick(e: MouseEvent): Unit = {
				val t: Tree = e.widget.asInstanceOf[Tree]
				t.getSelection flatMap (it => it(onDblClickHandler).map((it, _))) foreach (x => x._2.apply(x._1))
			}
			override def mouseDown(e: MouseEvent): Unit = {
				val t: Tree = e.widget.asInstanceOf[Tree]
				if (e.button == 3) {
					val selMenu = t.getSelection.headOption flatMap (_(ctxMenu))
					selMenu foreach { menu =>
						menu.setLocation(t.getDisplay.map(t, null, e.x, e.y))
						menu.setVisible(true)
					}
				}
			}
	}
	def register(t: Tree) {
		t.addTreeListener(TreeListener)
		t.addMouseListener(TreeListener)

		if (SWT.getPlatform() == "gtk")
			GTK.tightCells(t)
	}
}