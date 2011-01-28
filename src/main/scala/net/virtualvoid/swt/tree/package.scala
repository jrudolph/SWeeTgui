package net.virtualvoid.swt

import org.eclipse.swt.widgets.TreeItem
import tree.ItemDesc

package object tree {
  type ItemDecorator[T] = (T, TreeItem) => Unit

  def node[T]: ItemDesc[T] =
    ItemDesc((_, _) => ())
}