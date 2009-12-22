package net.virtualvoid.swt

import _root_.net.virtualvoid.scala.Tools._
import SWTTools._

import _root_.org.eclipse.swt
import swt.SWT
import swt.widgets.{ Tree, TreeItem, Menu, MenuItem }
import swt.events.{ TreeEvent, TreeListener, MouseEvent, MouseAdapter, SelectionAdapter, SelectionEvent }

object Trees {
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
	
	def ItemInfo[T](f: (ItemParent, T) => List[TreeItem]): ItemInfo[T] = new ItemInfo[T] {
		def create(it: ItemParent, obj: T): List[TreeItem] = f(it,obj)
	}
	trait ItemCreator[-T] {
		def create(it: ItemParent, obj: T): List[TreeItem]
	}
	trait ItemInfo[T] extends ItemCreator[T] {
		def sub(f: T => TreeItem => Unit): ItemInfo[T] = ItemInfo[T] { (parent, obj) => 
			val items = ItemInfo.this.create(parent, obj)
			items foreach f(obj)
			items
		}
		def labelled(label: T => String): ItemInfo[T] = sub(obj => it => it.setText(label(obj)))
		
		def as[U](descender: T => U)(i: => ItemCreator[U]): ItemInfo[T] = ItemInfo[T] { (parent, obj) =>
			i.create(parent, descender(obj))
		}
		def |-*[U](descender: T => Iterable[U])(f: ItemInfo[U] => ItemInfo[U]): ItemInfo[T] = sub { obj => it =>
			registerGenerator(it) {
				val child = f(item[U])			
				descender(obj) foreach (x => child.create(it, x))
			}
		}
		def |-!(label: T => String): ItemInfo[T] = |--(item[T] labelled label)
		def |--(i: => ItemCreator[T]): ItemInfo[T] = |--(x => x)(i)
		def |--[U](descender: T => U)(i: => ItemCreator[U]): ItemInfo[T] = sub { obj => it => registerGenerator(it)(i.create(it, descender(obj))) }
		
		def dblClick(handler: T => Unit) = sub { obj => it => it(onDblClickHandler) = ((_: TreeItem) => handler(obj)) }
		def contextMenu(label: T => String)(handler: T => Unit) = sub { obj => it => 
			val menu = ctxMenu.getOr(it)(new Menu(it.getParent))
			val item = new MenuItem(menu, SWT.PUSH)
			item.setText(label(obj))
			item.addSelectionListener(new SelectionAdapter {
				override def widgetSelected(s: SelectionEvent) { handler(obj) }
			})
		}
	}
	def item[T]: ItemInfo[T] = ItemInfo[T] { (parent, obj) =>
		List(createTreeItem(parent))
	}
	def choose[T, U <: T](func: PartialFunction[T,ItemCreator[U]]): ItemInfo[T] = ItemInfo[T] { (parent, obj) =>
		if (func.isDefinedAt(obj)) func(obj).create(parent, obj.asInstanceOf[U]) else List()
	}		

	implicit def str2ItemInfo[T](str: String): ItemInfo[T] = item[T] labelled(_ => str)
	implicit def liftConstantString[T](str: String): T => String = _ => str
	
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