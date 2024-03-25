package tasks.adts
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Sequences.Sequence

import scala.annotation.tailrec

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object Ex2SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object SchoolADT extends SchoolModule:
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


