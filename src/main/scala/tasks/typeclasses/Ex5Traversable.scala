package u04lab
import u03.Sequences.Sequence
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import u03.extensionmethods.Optionals.Optional

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  private def log[A](a: A): Unit = println("The next element is: "+a)

  @tailrec
  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()


  trait Traversable[T[_]]:
    def logValue[A](el: A): Unit
    def traverseStruct[A](struct: T[A]): Unit

  private def logAllValues[T[_]: Traversable, A](s: T[A]): Unit =
    val traversable = summon[Traversable[T]]
    traversable.traverseStruct(s)

  given Traversable[Sequence] with
    def logValue[A](el: A): Unit = println("Sequence Value: " + el)
    def traverseStruct[A](struct: Sequence[A]): Unit = struct match
      case Cons(h, tail) => this.logValue(h); this.traverseStruct(tail)
      case _ =>





  @main def tryTraversable(): Unit =
    val si = Cons(10, Cons(20, Cons(30, Nil())))
    logAllValues(si)