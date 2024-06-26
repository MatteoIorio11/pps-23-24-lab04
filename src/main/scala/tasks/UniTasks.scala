package tasks

import tasks.adts.Ex1ComplexNumbers.BasicComplexADT.ComplexImpl
import tasks.adts.Ex2SchoolModel.SchoolModule
import tasks.adts.Ex3Stacks.StackADT
import tasks.monads.Ex6TryModel.{Try, exec}
import u03.Optionals.Optional
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty
import u03.Sequences.Sequence
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import u04lab.Ex4Summables.Summable
import u03.extensionmethods.Optionals.Optional.None
import u04.monads.Monads.Monad
import u04.monads.States.State
import u04.monads.{CounterStateImpl, Monads, States, WindowStateImpl}

import scala.annotation.tailrec

object UniTasks:
  object Task1:
    private case class ComplexImpl(real: Double, imaginary: Double)

    opaque type Complex = ComplexImpl

    def complex(re: Double, im: Double): Complex = ComplexImpl(real = re, imaginary = im)

    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexImpl(real, _) => real
      def im(): Double = complex match
        case ComplexImpl(_, imaginary) => imaginary
      def sum(other: Complex): Complex =
        ComplexImpl(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex =
        ComplexImpl(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case ComplexImpl(re, im) if (re == 0 && im == 0) => re.toString
        case ComplexImpl(re, im) if (re != 0 && im == 0) => re.toString
        case ComplexImpl(re, im) if (re != 0 && im > 0) => re + " + " + im + "i"
        case ComplexImpl(re, im) if (re != 0 && im < 0) => re + " - " + Math.abs(im) + "i"
        case ComplexImpl(re, im) if (re == 0 && im > 0) => im + "i"
        case ComplexImpl(re, im) if (re == 0 && im < 0) => im + "i"

  object Task2 extends SchoolModule:
    private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])
    private case class TeacherImpl(name: String, courses: Sequence[Course])
    private case class CourseImpl(name: String)

    opaque type School = SchoolImpl
    opaque type Teacher = TeacherImpl
    opaque type Course = CourseImpl

    def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)

    def teacher(name: String): Teacher = TeacherImpl(name, Nil())

    def course(name: String): Course = CourseImpl(name)

    extension (school: School)
      def addTeacher(name: String): School = school match
        case SchoolImpl(ts, cs) => SchoolImpl(Cons(TeacherImpl(name, Nil()), ts), cs)

      def addCourse(name: String): School = school match
        case SchoolImpl(ts, cs) => SchoolImpl(ts, Cons(CourseImpl(name), cs))
      def teacherByName(name: String): Optional[Teacher] = school match
        case SchoolImpl(ts, _) => _findTeacher(ts, name)
      @tailrec
      private def _findTeacher(seq: Sequence[Teacher], name: String): Optional[Teacher] = seq match
        case Nil() => Optional.Empty()
        case Cons(TeacherImpl(teachersName, courses), tail) if teachersName.eq(name) => Optional.Just(TeacherImpl(teachersName, courses))
        case Cons(_, t) => _findTeacher(t, name)

      def courseByName(name: String): Optional[Course] = school match
        case SchoolImpl(_, cs) => _findCourse(cs, name)
      @tailrec
      private def _findCourse(seq: Sequence[Course], name: String): Optional[Course] = seq match
        case Nil() => Optional.Empty()
        case Cons(CourseImpl(coursesName), ts) if coursesName.eq(name) => Optional.Just(CourseImpl(coursesName))
        case Cons(_, ts) => _findCourse(ts, name)
      def nameOfTeacher(teacher: Teacher): String = teacher.name

      def nameOfCourse(course: Course): String = course.name
      def setTeacherToCourse(teacher: Teacher, course: Course): School = school match
        case SchoolImpl(ts, cs) => SchoolImpl(Sequence.map(ts)(t => if t.name == teacher.name then TeacherImpl(t.name, Cons(course, t.courses)) else t), cs)

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        school.teacherByName(teacher.name) match
          case Optional.Empty() => Nil()
          case Optional.Just(t) => t.courses

  object Task3 extends StackADT:
    private case class MyStack[A](sequence: Sequence[A])

    opaque type Stack[A] = MyStack[A]

    def empty[A]: Stack[A] = MyStack(Nil())

    extension[A] (stack: Stack[A])
      def push(a: A): Stack[A] =
        val seq = stack.sequence
        seq match
          case Nil() => MyStack(Cons(a, Nil()))
          case Cons(h, tail) => MyStack(Cons(a, Cons(h, tail)))
      def pop(a: A): Optional[(A, Stack[A])] = _popValues(a, stack.sequence)
      @tailrec
      private def _popValues(a: A, values: Sequence[A]): Optional[(A, Stack[A])] = values match
        case Nil() => Optional.Empty()
        case Cons(h, tail) if (h == a) => Optional.Just((h, MyStack(tail)))
        case Cons(h, tail) => _popValues(a, tail)
      def asSequence(): Sequence[A] = stack.sequence

  object Task4:
    def sumAll[A: Summable](seq: Sequence[A]): A =
      val summable = summon[Summable[A]]
      seq match
        case Nil() => summable.zero
        case Cons(h, t) => summable.sum(h, sumAll(t))

    given Summable[Int] with
      def sum(a1: Int, a2: Int): Int = a1 + a2
      def zero: Int = 0

    given Summable[Double] with
      def sum(a1: Double, a2: Double): Double = a1 + a2
      def zero: Double = 0.0

    given Summable[String] with
      def sum(a1: String, a2: String): String = a1 + a2
      def zero: String = ""

  object Task5:
    trait Traversable[T[_]]:
      def logValue[A](el: A): Unit

      def traverseStruct[A](struct: T[A]): Unit

    private def logAllValues[T[_] : Traversable, A](s: T[A]): Unit =
      val traversable = summon[Traversable[T]]
      traversable.traverseStruct(s)

    given Traversable[Sequence] with
      def logValue[A](el: A): Unit = println("Sequence Value: " + el)

      def traverseStruct[A](struct: Sequence[A]): Unit = struct match
        case Cons(h, tail) => this.logValue(h); this.traverseStruct(tail)
        case _ =>

    given Traversable[Optional] with
      def logValue[A](el: A): Unit = println("Optional Value: " + el)
      def traverseStruct[A](struct: Optional[A]): Unit = struct match
        case Just(el) => this.logValue(el)
        case None() =>
  object Task6:
    private enum TryImpl[A]:
      case Success(value: A)
      case Failure(exception: Throwable)

    opaque type Try[A] = TryImpl[A]

    def success[A](value: A): Try[A] = TryImpl.Success(value)

    def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)

    def exec[A](expression: => A): Try[A] = try success(expression) catch
      case e: Throwable => failure(e)
    given Monad[Try] with
      override def unit[A](value: A): Try[A] = exec(value)
      extension[A] (m: Try[A])
        override def flatMap[B](f: A => Try[B]): Try[B] = m match
          case TryImpl.Success(value) => f(value)
          case TryImpl.Failure(exception) => TryImpl.Failure(exception)

  object Task7:
    def runMVC =
      import Monads.*, Monad.*, States.*, State.*, CounterStateImpl.*, WindowStateImpl.*
      import u03.extensionmethods.Streams.*

      def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
        State: (sm, sv) =>
          val (sm2, am) = m1.run(sm)
          val (sv2, av) = f(am).run(sv)
          ((sm2, sv2), av)

      def windowCreation(str: String): State[Window, Stream[String]] = for
        _ <- setSize(300, 300)
        _ <- addButton(text = "inc", name = "IncButton")
        _ <- addButton(text = "dec", name = "DecButton")
        _ <- addButton(text = "reset", name = "ResetButton")
        _ <- addButton(text = "quit", name = "QuitButton")
        _ <- addButton(text = "set", name = "SetButton")
        _ <- addTextField("TextField")
        _ <- addLabel(text = str, name = "Label1")
        _ <- show()
        events <- eventStream()
      yield events

      val controller = for
        events <- mv(seq(reset(), get()), i => windowCreation(i.toString()))
        _ <- seqN(events.map(_ match
          case "SetButton" => mv(nop(), i => toLabelFromTextField("TextField", "Label1"))
          case "IncButton" => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
          case "DecButton" => mv(seq(dec(), get()), i => toLabel(i.toString, "Label1"))
          case "ResetButton" => mv(seq(reset(), get()), i => toLabel(i.toString, "Label1"))
          case "QuitButton" => mv(nop(), _ => exec(sys.exit()))))
      yield ()

      controller.run((initialCounter(), initialWindow))