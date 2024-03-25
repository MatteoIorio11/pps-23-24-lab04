package tasks.adts

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import tasks.adts.Ex3Stacks.{StackADT, StackImpl}

class StackTests {
  val stackADT: StackADT = StackImpl
  import stackADT.*
  val myStack: Stack[Int] = stackADT.empty

  @Test def testPush(): Unit =
    assertEquals(empty.push(1), myStack.push(1))
}
