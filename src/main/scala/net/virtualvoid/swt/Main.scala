package net.virtualvoid.swt

import _root_.org.eclipse.swt
import swt.SWT
import swt.widgets.{Widget, Shell, Tree, TreeItem}
import swt.events.{TreeAdapter, TreeEvent}
import swt.layout.FillLayout

object Main {
	trait Do[T] {
		/** execute for side-effects and return */
		def ~!(f: T => Unit): T
	}
	implicit def anyRef2Do[T <: AnyRef](t: T): Do[T] = new Do[T]{
		def ~!(f: T => Unit): T = { f(t); t}
	}
	implicit def toLeft[T,U](t: T) = Left(t)
	implicit def toRight[T,U](u: U) = Right(u)
	
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
	
	type ItemParent = Either[Tree, TreeItem]
	/** Account for the missing common ancestor by using Either */
	def createTreeItem(parent: ItemParent): TreeItem = parent match {
		case Left(t) => new TreeItem(t, SWT.NONE)
		case Right(item) => new TreeItem(item, SWT.NONE)
	}
	
	def children[T <: AnyRef, U <: AnyRef](f: T => List[U])(creator: ItemParent => U => TreeItem)
		: ItemParent => T => List[TreeItem] = parent => o => f(o).map(creator(parent))
		
	type ItemCreator[T] = ItemParent => T => TreeItem
	def item[T](label: T => String = (_: T).toString): ItemCreator[T] =
		parent => o => createTreeItem(parent) ~! (_.setText(label(o)))
	/*
	def item[T, U](label: T => String = (_: T).toString
			                             ,children: ItemParent => T => List[TreeItem] = (_: ItemParent) => (_: T) => Nil)
		: ItemCreator[T] = { parent => o =>
		(createTreeItem(parent)   
			~! (_.setText(label(o))) 
			~! (children(_)(o)))
	}
	 */
	val childrenGenerator = ItemStorage[(TreeItem, () => Unit)]("children")
	def parent[T, U](creator: ItemCreator[T])(children: T => Iterable[U])(childCreator: => ItemCreator[U]): ItemCreator[T] =
		parent => o => {
			val item = creator(parent)(o)
			item(childrenGenerator) = (new TreeItem(item, SWT.NONE) ~! (_.setText("dummy")), () => children(o) foreach childCreator(item))
			item
		}
		
	case class BoundField(o: AnyRef, field: java.lang.reflect.Field)
	def main(args: Array[String]){
		val shell = new Shell
		shell.setLayout(new FillLayout)
		val tree = new Tree(shell, SWT.NONE)
		tree.addTreeListener(new TreeAdapter {
			override def treeExpanded(e: TreeEvent): Unit = 
				for ((dummy, childrenGen) <- e.item(childrenGenerator)) {
					childrenGenerator.clear(e.item)
					childrenGen()
					dummy.dispose
				}
		})
		
		def oneObject: ItemCreator[AnyRef] = 
			parent(item[AnyRef](_.toString()))(x => x.getClass.getFields.map(f => BoundField(x, f))){
				parent(item((_:BoundField).field.getName))(x => List(x.field.get(x.o))) {
					oneObject
				}
			}
		
		oneObject(tree)("Wurst")
		
		shell.open
		val display = shell.getDisplay
		while (!shell.isDisposed ()) {
			if (!display.readAndDispatch ()) display.sleep ();
		}

	}
}
