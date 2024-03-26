package tasks.mvc
import u04.monads.*
object Ex8DrawMyNumber extends App{
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
      _ <- addButton(text = "try", name = "TryButton")
      _ <- addTextField("TextField")
      _ <- addLabel(text = str, name = "Label1")
      _ <- show()
      events <- eventStream()
    yield events

    val controller = for
      events <- mv(seq(reset(), get()), i => windowCreation(i.toString()))
      _ <- seqN(events.map(_ match
        case "TryButton" => mv(nop(), i => toLabelFromTextField("TextField", "Label1"))))
    yield ()

    controller.run((initialCounter(), initialWindow))

  runMVC
}
