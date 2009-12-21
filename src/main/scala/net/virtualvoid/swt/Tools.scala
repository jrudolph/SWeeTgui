package net.virtualvoid.swt

object Tools {
	trait Do[T] {
		/** execute for side-effects and return */
		def ~!(f: T => Unit): T
	}
	implicit def anyRef2Do[T <: AnyRef](t: T): Do[T] = new Do[T]{
		def ~!(f: T => Unit): T = { f(t); t}
	}
	implicit def toLeft[T,U](t: T) = Left(t)
	implicit def toRight[T,U](u: U) = Right(u)
}
