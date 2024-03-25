package tasks.adts

import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]
  
  object StackImpl extends StackADT:
    private case class MyStack[A](sequence: Sequence[A])
    opaque type Stack[A] = MyStack[A]
    def empty[A]: Stack[A] = MyStack(Nil())
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] =
        val seq = stack.sequence
        seq match
          case Nil() => MyStack(Cons(a, Nil()))
          case Cons(h, tail) => MyStack(Cons(a, Cons(h, tail)))
      def pop(a: A): Optional[(A, Stack[A])] = _popValues(a, stack.sequence)
        private def _popValues(a: A, values: Sequence[A]): Optional[(A, Stack[A])] = values match
          case Nil() => Optional.Empty()
          case Cons(h, tail) if (h == a) => Optional.Just((h, MyStack(tail)))
          case Cons(h, tail) => _popValues(a, tail)
      def asSequence(): Sequence[A] = ???