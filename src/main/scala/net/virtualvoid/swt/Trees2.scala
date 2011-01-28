package net.virtualvoid.swt

import net.virtualvoid.swt.Trees2.ItemDecorator
import org.eclipse.swt.widgets.{Tree, TreeItem}
import org.eclipse.swt.SWT

object Trees2 {
  type ItemDecorator[T] = (T, TreeItem) => Unit

  trait ItemDesc[T] extends ItemDecorator[T] { first =>
    def create(value: T, parent: TreeItem) = apply(value, new TreeItem(parent, SWT.NONE))
    def create(value: T, parent: Tree) = apply(value, new TreeItem(parent, SWT.NONE))

    def decorateWith[T0 <: T](next: ItemDecorator[T0]): ItemDesc[T0] =
      (value: T0, item: TreeItem) => {
        first(value, item)
        next(value, item)
      }

    def label(labeller: T => String): ItemDesc[T] =
      decorateWith((value, item) => item.setText(labeller(value)))
    def child[U](f: T => U)(decorator: => ItemDecorator[U]): ItemDesc[T] =
      decorateWith((value, item) =>
          Trees.registerGenerator(item)(node[U].decorateWith(decorator).create(f(value), item))
      )
    def children[U](f: T => Traversable[U])(decorator: => ItemDecorator[U]): ItemDesc[T] =
      decorateWith((value, item) =>
          Trees.registerGenerator(item)(f(value).foreach(v => node[U].decorateWith(decorator).create(v, item)))
      )
  }
  object ItemDesc {
    def apply[T](f: (T, TreeItem) => Unit): ItemDesc[T] =
      new ItemDesc[T] {
        def apply(value: T, item: TreeItem): Unit = f(value, item)
      }
    implicit def funcIsItemDesc[T](f: (T, TreeItem) => Unit): ItemDesc[T] = apply(f)
  }

  def node[T]: ItemDesc[T] =
    ItemDesc((_, _) => ())
}