package lib


object Empty:
  @inline final def apply[A](using empty: Empty[A]): Empty[A] = empty

trait Empty[A]:
    def empty: A
