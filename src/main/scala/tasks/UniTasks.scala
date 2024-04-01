package tasks

import tasks.adts.Ex1ComplexNumbers.BasicComplexADT.ComplexImpl
import tasks.adts.Ex2SchoolModel.SchoolModule
import u03.Optionals.Optional
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty
import u03.Sequences.Sequence
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil

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

  