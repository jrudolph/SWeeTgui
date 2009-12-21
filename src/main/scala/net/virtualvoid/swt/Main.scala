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
	val childrenGenerator = ItemStorage[(TreeItem, () => Unit)]("children")
	
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
			
		def |-*[U](descender: T => Iterable[U])(f: ItemInfo[U] => ItemInfo[U]): ItemInfo[T] = sub { obj => it =>
			registerGenerator(it) {
				val child = f(item[U])			
				descender(obj) foreach (x => child.create(it, x))
			}
		}
		def |-!(label: T => String): ItemInfo[T] = |--(item[T] labelled label)
		def |--(i: => ItemCreator[T]): ItemInfo[T] = |--(x => x)(i)
		def |--[U](descender: T => U)(i: => ItemCreator[U]): ItemInfo[T] = sub { obj => it => registerGenerator(it)(i.create(it, descender(obj))) }
		
	}
	def item[T]: ItemInfo[T] = ItemInfo[T] { (parent, obj) =>
		List(createTreeItem(parent))
	}
	implicit def str2ItemInfo[T](str: String): ItemInfo[T] = item[T] labelled(_ => str) 
	
	case class BoundField(o: AnyRef, field: java.lang.reflect.Field)
	def fieldsOf(o: AnyRef) = o.getClass.getFields.map(BoundField(o, _))
	case class BoundMethod0(o: AnyRef, method: java.lang.reflect.Method)
	def methodsOf(o: AnyRef) = o.getClass.getMethods.filter(m => m.getParameterTypes.length == 0 && m.getReturnType != classOf[Void]).map(BoundMethod0(o, _))
	
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
