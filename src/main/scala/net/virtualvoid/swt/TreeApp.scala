package net.virtualvoid.swt

import Trees._
import _root_.net.virtualvoid.scala.Tools._
import _root_.org.eclipse.swt
import swt.SWT
import swt.layout.FillLayout
import swt.widgets.{ Shell, Tree }

trait TreeApp[T] {
	def treeCreator: ItemCreator[T]
	def startObject: T
	
	def run {
		val shell = new Shell
		shell.setLayout(new FillLayout)
		val tree = new Tree(shell, SWT.NONE)
		tree.addTreeListener(TreeExpansionListener)

		treeCreator.create(tree, startObject)

		shell.open
		val display = shell.getDisplay
		while (!shell.isDisposed) {
			if (!display.readAndDispatch) display.sleep
		}
	}
}