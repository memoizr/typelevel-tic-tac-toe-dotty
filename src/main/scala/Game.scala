
sealed trait Cell {
  type OtherPlayer <: Cell
  type validate[Other <: Cell] <: Cell

  type isX[ifX <: Bool, ifO <: Bool] <: Bool
  type isEq[A <: Cell] <: Bool
}

trait X extends Cell {
  override type OtherPlayer = O
  override type validate[Other <: Cell] = X

  type isX[ifX <: Bool, ifO <: Bool] = ifX
  type isEq[A <: Cell] = A#isX[True, False]
}

trait O extends Cell {
  override type OtherPlayer = X
  override type validate[Other <: Cell] = O

  type isX[ifX <: Bool, ifO <: Bool] = ifO
  type isEq[A <: Cell] = A#isX[False, True]
}

trait N extends Cell {
  override type OtherPlayer = N
  override type validate[Other <: Cell] = N | Other

  type isX[ifX <: Bool, ifO <: Bool] = False
  type isEq[A <: Cell] <: False
}


class Game[
A <: Cell, B <: Cell, C <: Cell,
D <: Cell, E <: Cell, F <: Cell,
G <: Cell, H <: Cell, I <: Cell,
current <: Cell] {
  type winner <: Cell

  def play[
  A1 <: A#validate[current], B1 <: B#validate[current], C1 <: C#validate[current],
  D1 <: D#validate[current], E1 <: E#validate[current], F1 <: F#validate[current],
  G1 <: G#validate[current], H1 <: H#validate[current], I1 <: I#validate[current]
  ]() = {

    new Game
      [A1, B1, C1,
        D1, E1, F1,
        G1, H1, I1,
        current#OtherPlayer] {
      //    type winner = allEqual[A1, B1, C1]#If[A1, A1, Cell]
      type winner =
      A1#isEq[B1]#AND[B1#isEq[C1]]#If[
        A1,
        D1#isEq[E1]#AND[E1#isEq[F1]]#If[
          D1,
          G1#isEq[H1]#AND[H1#isEq[I1]]#If[
            G1,
            A1#isEq[D1]#AND[D1#isEq[G1]]#If[
              A1,
              B1#isEq[E1]#AND[E1#isEq[H1]]#If[
                B1,
                C1#isEq[F1]#AND[F1#isEq[I1]]#If[
                  C1,
            N, Cell], Cell], Cell], Cell], Cell], Cell]
    }
  }
}

