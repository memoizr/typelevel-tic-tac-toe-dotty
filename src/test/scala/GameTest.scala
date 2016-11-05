import org.scalatest.{FlatSpec, Matchers}


class GameTest extends FlatSpec {


  import Winner._
  it should "win" in {
    val game = new Game[
      N, N, N,
      N, N, N,
      N, N, N,
      O].play[
      O, N, N,
      N, N, N,
      N, N, N
      ]().play[
      O, X, N,
      N, N, N,
      N, N, N
      ]().play[
      O, X, N,
      O, N, N,
      N, N, N
      ]().play[
      O, X, N,
      O, N, N,
      N, X, N
      ]().play[
      O, X, N,
      O, N, O,
      N, X, N
      ]().play[
      O, X, N,
      O, X, O,
      N, X, N
      ]()
    val winner = getWinner[game.type#winner]
    println(winner.str)
  }
}


object Winner {
  case class WinnerRep[C <: Cell](str: String)
  implicit val xWins: WinnerRep[X] = WinnerRep("x")
  implicit val oWins: WinnerRep[O] = WinnerRep("O")
  implicit val nWins: WinnerRep[N] = WinnerRep("N")

  def getWinner[A <: Cell : WinnerRep] = implicitly[WinnerRep[A]]
}
