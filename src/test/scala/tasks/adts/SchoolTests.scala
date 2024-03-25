package tasks.adts

import org.junit.jupiter.api.Test
import org.junit.Assert.*
import tasks.adts.Ex2SchoolModel.SchoolADT
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Sequences.Sequence

class SchoolTests {
  val schoolADT: Ex2SchoolModel.SchoolADT.type = SchoolADT
  import schoolADT.*
  val generalSchool: School = school(Nil(), Nil())

  @Test def testAddTeacher(): Unit =
    val t: Teacher = teacher("mario")
    val expected: School = school(Cons(t, Nil()), Nil())
    assertEquals(expected, generalSchool.addTeacher("mario"))

  @Test def testAddCourse(): Unit =
    val c = course("pps")
    val expected: School = school(Nil(), Cons(c, Nil()))
    assertEquals(expected, generalSchool.addCourse("pps"))

  @Test def testGetTeacherByName(): Unit =
    val t = teacher("mario")
    val school = generalSchool.addTeacher("mario")
    assertEquals(Optional.Just(t), school.teacherByName("mario"))

}
