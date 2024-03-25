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
      def addCourse(name: String, teacher: Teacher): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object SchoolADT extends SchoolModule:
    private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])
    private case class TeacherImpl(name: String)
    private case class CourseImpl(name: String, teacher: Teacher)
    opaque type School = SchoolImpl
    opaque type Teacher = TeacherImpl
    opaque type Course = CourseImpl

    def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)
    def teacher(name: String): Teacher = TeacherImpl(name)
    def course(name: String, teacher: Teacher): Course = CourseImpl(name, teacher)
    extension (school: School)
      def addTeacher(name: String): School = school match
        case SchoolImpl(ts, cs) => SchoolImpl(Cons(TeacherImpl(name), ts), cs)

      def addCourse(name: String, teacher: Teacher): School = school match
        case SchoolImpl(ts, cs) => SchoolImpl(ts, Cons(CourseImpl(name, teacher), cs))
      def teacherByName(name: String): Optional[Teacher] = school match
        case SchoolImpl(ts, _) => _findTeacher(ts, name)
        @tailrec
        private def _findTeacher(seq: Sequence[Teacher], name: String): Optional[Teacher] = seq match
          case Nil() => Optional.Empty()
          case Cons(TeacherImpl(teachersName), tail) if teachersName.eq(name) => Optional.Just(TeacherImpl(teachersName))
          case Cons(_, t) => _findTeacher(t, name)

      def courseByName(name: String): Optional[Course] = school match
        case SchoolImpl(_, cs) => _findCourse(cs, name)
        @tailrec
        private def _findCourse(seq: Sequence[Course], name: String): Optional[Course] = seq match
          case Nil() => Optional.Empty()
          case Cons(CourseImpl(coursesName, teacher), ts) if coursesName.eq(name) => Optional.Just(CourseImpl(coursesName, teacher))
          case Cons(_, ts) => _findCourse(ts, name)
      def nameOfTeacher(teacher: Teacher): String = teacher.name

      def nameOfCourse(course: Course): String = course.name
      def setTeacherToCourse(teacher: Teacher, course: Course): School = school match
        case SchoolImpl(ts, cs) => SchoolImpl(ts, _changeTeacher(course, teacher, cs))
        @tailrec
        private def _changeTeacher(course: Course, teacher: Teacher, courses: Sequence[Course]): Sequence[Course] = courses match
          case Cons(CourseImpl(cName, t), tail) if cName.eq(course.name) => Cons(CourseImpl(course.name, teacher), tail)
          case Cons(h, tail) => _changeTeacher(course, teacher, tail)
          case Nil() => Nil()
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = 


