package tasks.adts

import org.junit.jupiter.api.Test
import org.junit.Assert.*
import tasks.adts.Ex2SchoolModel.SchoolADT
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Sequences.Sequence

class SchoolTests {
  val schoolADT = SchoolADT
  import schoolADT.*
  val generalSchool = school(Nil(), Nil())

  @Test def testAddTeacher(): Unit =
    val t: Teacher = teacher("mario")
    val expected: School = school(Cons(t, Nil()), Nil())
    assertEquals(expected, generalSchool.addTeacher("mario"))




}
