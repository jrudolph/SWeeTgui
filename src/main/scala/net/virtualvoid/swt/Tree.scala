package net.virtualvoid.swt

import _root_.net.virtualvoid.scala.Tools._
import SWTTools._

import _root_.org.eclipse.swt
import swt.SWT
import swt.widgets.{ Tree, TreeItem }
import swt.events.{ TreeEvent, TreeListener, MouseEvent, MouseAdapter }

object Trees {
	type ItemParent = Either[Tree, TreeItem]
	/** Account for the missing common ancestor by using Either */
	def createTreeItem(parent: ItemParent): TreeItem = parent match {
		case Left(t) => new TreeItem(t, SWT.NONE)
		case Right(item) => new TreeItem(item, SWT.NONE)
	}
	val childrenGenerator = ItemStorage[(TreeItem, () => Unit)]("children")
	val onDblClickHandler = ItemStorage[TreeItem => Unit]("dblClickHandler")
	
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
	}
	def register(t: Tree) {
		t.addTreeListener(TreeListener)
		t.addMouseListener(TreeListener)
	}
}
