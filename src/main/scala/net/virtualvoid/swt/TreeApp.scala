package net.virtualvoid.swt

import _root_.org.eclipse.swt
import swt.SWT
import swt.layout.FillLayout
import swt.widgets.{ Shell, Tree }
import net.virtualvoid.swt.Trees2.ItemDesc

trait TreeApp[T] {
  def treeCreator: ItemDesc[T]
  def startObject: T
  def title: String = "Generic TreeApp"

  def run {
    val shell = new Shell
    shell.setLayout(new FillLayout)
    shell.setText(title)
    val tree = new Tree(shell, SWT.NONE)
    Trees.register(tree)

    treeCreator.create(startObject, tree)

    shell.open
    val display = shell.getDisplay
    while (!shell.isDisposed) {
      if (!display.readAndDispatch) display.sleep
    }
  }

  def main(args: Array[String]) = run
}