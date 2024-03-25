package tasks.adts

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import tasks.adts.Ex3Stacks.{StackADT, StackImpl}
import u03.Optionals.Optional

class StackTests {
  val stackADT: StackADT = StackImpl
  import stackADT.*
  val myStack: Stack[Int] = stackADT.empty

  @Test def testPush(): Unit =
    assertEquals(empty.push(1), myStack.push(1))

  @Test def testPop(): Unit =
    val stack = myStack.push(1).push(2).push(3)
    val opt = stack.pop(3)
    assertEquals(Optional.Just((3, empty.push(1).push(2))), opt)
}
