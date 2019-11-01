package net.degoes.zio

import java.io.IOException
import java.nio.charset.StandardCharsets

import zio._
import zio.console.Console

object Notes extends App {
  def putStrLn2(line: String): ZIO[Console, Nothing, Unit] =
    ZIO.accessM[Console](_.console.putStrLn(line))

  val written: ZIO[Any, Nothing, Unit] =
    putStrLn2("hell").provide(new Console {
      val console = new Console.Service[Any] {
        override def putStr(line: String): ZIO[Any, Nothing, Unit] = ???

        override def putStrLn(line: String): ZIO[Any, Nothing, Unit] = ???

        override val getStrLn: ZIO[Any, IOException, String] = ???
      }
    })

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = ???
}

object HelloWorld extends App {
  import zio.console._

  /**
    * EXERCISE 1
    *
    * Implement a simple "Hello World" program using the effect returned by `putStrLn`.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello World!") *>
      putStrLn("Goodbye World!") *>
      ZIO.succeed(0)
}

object ErrorConversion extends App {
  val StdInputFailed = 1

  import zio.console._

  val failed =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!")

  /**
    * EXERCISE 2
    *
    * Using `ZIO#orElse` or `ZIO#fold`, have the `run` function compose the
    * preceding `failed` effect into the effect that `run` returns.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    failed.fold(_ => 1, _=> 0)
    //failed map (_ => 0) orElse ZIO.succeed(1)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  /**
    * EXERCISE 3
    *
    * Implement a simple program that asks the user for their name (using
    * `getStrLn`), and then prints it out to the user (using `putStrLn`).
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _     <- putStrLn("Please enter your name: ")
      name  <- getStrLn
      _     <- putStrLn(s"Hello, $name!")
    } yield ()).fold(_ => StdInputFailed, _ => 0)
}

object ZIOTypes {
  type ??? = Nothing

  /**
    * ZIO[-R, +E, +A]
    *
    * A = The type with which the effect may succeed
    *     (Corresponds to the `A` in `Future[A]`)
    *
    * E = The type with which the effect may failed
    *     (Corresponds to the `Throwable`)
    *
    * R = The environment type required to run the effect
    * R => Either(E, A]
    *
    * If we don't need R, we can use Any
    * _           wan't to fail or succeed, we can use Nothing
    */

  /**
    * EXERCISE 4
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A] = ZIO[Any, Throwable, A]
  type UIO[+A] = ZIO[Any, Nothing, A]
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object NumberGuesser extends App {
  import zio.console._
  import zio.random._

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn("You did not guess correctly. The answer was ${random}")

  /**
    * EXERCISE 5
    *
    * Choose a random number (using `nextInt`), and then ask the user to guess
    * the number, feeding their response to `analyzeAnswer`, above.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      random <- nextInt(10)
      _      <- putStrLn("Guess a number from 0 to 10: ")
      guess  <- getStrLn
      _      <- analyzeAnswer(random, guess)
    } yield ()).fold(_ => 1, _ => 0)
}

object AlarmApp extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException

  /**
    * EXERCISE 6
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a decimal number of seconds.
    */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      for {
        double <- ZIO.effect(input.toDouble).refineToOrDie[NumberFormatException]
      } yield (double * 1000.0).toInt.milliseconds

    def fallback(input: String): ZIO[Console, IOException, Duration] =
      parseDuration(input).catchAll(error =>
        putStrLn(s"You enter $input, which is not a decimal: ${error.toString}") *>
          getAlarmDuration
      )
      // parseDuration(input).foldM(
      //   error => putStrLn(s"You enter $input, which is not a decimal: ${error.toString}") *>
      //   getAlarmDuration,
      //   duration => ZIO.succeed(duration)
      // )
      // parseDuration(input).tapError(error =>
      //   putStrLn(s"You enter $input, which is not a decimal: ${error.toString}")
      // ) orElse getAlarmDuration*/

    for {
      _        <- putStrLn("Please enter the number of seconds to sleep (e.g. 1.5)")
      input    <- getStrLn
      duration <- fallback(input)
    } yield duration
  }

  /**
    * EXERCISE 7
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds, and then prints out a wakeup
    * alarm message.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _        <- putStrLn("Welcome to ZIO Alarm!")
      duration <- getAlarmDuration
      _        <- putStrLn(s"You will be awoken after $duration")
      _        <- ZIO.sleep(duration)
      _        <- putStrLn("Time to wake up!")
    } yield ()).fold(_ => 1, _ => 0)
}

object Cat extends App {
  import zio.console._
  import zio.blocking._
  import java.io.IOException
  import scala.io.Source

  /**
    * EXERCISE 8
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFile(file: String): ZIO[Blocking, IOException, String] =
    blocking {
      ZIO.effect(Source.fromFile(file).mkString("\n"))
        .refineToOrDie[IOException]
    }


  /**
    * EXERCISE 9
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (args match {
        case Nil => putStrLn("You did not enter a file path to show!")
        case _ :: file :: _ => readFile(file).flatMap(putStrLn(_))
    }).fold(_ => 1, _=> 0)
}

object CatIncremental extends App {
  import zio.console._
  import zio.blocking._
  import java.io._

  /**
    * EXERCISE 10
    *
    * Implement all missing methods of `FileHandle`. Be sure to do all work on
    * the blocking thread pool.
    */
  final case class FileHandle private (private val is: InputStream) {
    final def close: ZIO[Blocking, IOException, Unit] =
      blocking {
        ZIO.effect(is.close()).refineToOrDie[IOException]
      }

    final def read: ZIO[Blocking, IOException, Option[Chunk[Byte]]] =
      blocking {
        ZIO.effect {
          val array = Array.ofDim[Byte](1024)
          val read = is.read(array)

          if (read == -1) None
          else Some(Chunk.fromArray(array).take(read))
        }.refineToOrDie[IOException]
      }
  }
  object FileHandle {
    final def open(file: String): ZIO[Blocking, IOException, FileHandle] =
      blocking {
        ZIO.effect {
          new FileHandle(new FileInputStream(new File(file)))
        }.refineToOrDie[IOException]
      }
  }

  /**
    * EXERCISE 11
    *
    * Implement an incremental version of the `cat` utility, using `ZIO#bracket`
    * or `ZManaged` to ensure the file is closed in the event of error or
    * interruption.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (args match {
      case Nil => putStrLn("You did not enter a file path to show!")
      case _ :: file :: _ =>
        val managedFile = ZManaged.make(FileHandle.open(file))(_.close.orDie)
        managedFile.use { handle =>
          lazy val loop: ZIO[zio.console.Console with Blocking, IOException, Unit] = handle.read.flatMap {
            case None => ZIO.succeed(())
            case Some(chunk) => putStrLn(new String(chunk.toArray, StandardCharsets.UTF_8)) *> loop
          }

          loop
        }
    }).fold(_ => 1, _ => 0)

    // (args match {
    //   case Nil => putStrLn("You did not enter a file path to show!")
    //   case _ :: file :: _ =>
    //     FileHandle.open(file).bracket(_.close.orDie) { handle =>
    //       lazy val loop: ZIO[zio.console.Console with Blocking, IOException, Unit] = handle.read.flatMap {
    //         case None => ZIO.succeed(())
    //         case Some(chunk) => putStrLn(new String(chunk.toArray, StandardCharsets.UTF_8)) *> loop
    //       }
    //
    //       loop
    //     }
    // }).fold(_ => 1, _ => 0)
}

/**
  * # note
  *
  * Ref can be seen as a concurrent safe version of var
  * more: all methods of Ref are lazy
  */
object ComputePi extends App {
  import zio.random._
  import zio.console._

  /**
    * Some state to keep track of all points inside a circle,
    * and total number of points.
    */
  final case class PiState(
      inside: Ref[Long],
      total: Ref[Long]
  )

  /**
    * A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
    * A helper function that determines if a point lies in
    * a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
    * An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  def sample(state: PiState, iterations: Int): ZIO[Random, Nothing, Unit] =
    if (iterations <= 0) ZIO.unit // equivalent to ZIO.f(())
    else (for {
      random  <- randomPoint
      (x, y)  = random
      _       <- state.total.update(_ + 1)
      _       <- if (insideCircle(x, y)) state.inside.update(_ + 1) else ZIO.unit
    } yield ()) *> sample(state, iterations - 1)

  /**
    * EXERCISE 12
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      total   <- Ref.make(0L)
      inside  <- Ref.make(0L)
      state   =  PiState(inside, total)
      worker  = sample(state, 100000)
      _       <- ZIO.collectAllPar(List.fill(4)(worker)) // Lookup why collectAllPar is more efficient than collectAll
      tuple   <- inside.get zip total.get
      _       <- putStrLn(s"The value of pi is approximately: ${estimatePi(tuple._1, tuple._2)}")
    } yield ()).fold(_ => 1, _=> 0)
}

object Hangman extends App {
  import zio.console._
  import zio.random._
  import java.io.IOException
  import net.degoes.zio.Hangman.GuessResult.{ Correct, Incorrect, Won, Lost, Unchanged }

  /**
    * EXERCISE 13
    *
    * Implement an effect that gets a single, lower-case character from
    * the user.
    */
  lazy val getChoice: ZIO[Console, Nothing, Char] =
    getStrLn.orDie.map(_.trim.toLowerCase.toList).flatMap {
      case char :: Nil if char.isLetter => ZIO.succeed(char)
      case _ => putStrLn("You did not enter a single character!") *> getChoice
    }

  /**
    * EXERCISE 14
    *
    * Implement an effect that prompts the user for their name, and
    * returns it.
    */
  lazy val getName: ZIO[Console, IOException, String] =
    putStrLn("Please enter your name: ") *> getStrLn

  /**
    * EXERCISE 15
    *
    * Implement an effect that chooses a random word from the dictionary.
    */
  lazy val chooseWord: ZIO[Random, Nothing, String] = {
    val dictionary = Dictionary.Dictionary
    nextInt(dictionary.length).map(dictionary(_))
  }

  /**
    * EXERCISE 17
    *
    * Implement the main game loop, which gets choices from the user until
    * the game is won or lost.
    */
  def gameLoop(state0: State): ZIO[Console, IOException, Unit] =
    (for {
      char <- getChoice
      state = state0.addChar(char)
      State(name, _, word) = state
      _ <- renderState(state)
      loop <- guessResult(state0, state, char) match {
        case Won => putStrLn(s"Congratulations, $name, you won!!") as false
        case Lost => putStrLn(s"Sorry, $name, you lost. The word was $word.") as false
        case Unchanged => putStr("Sorry, $name, you already guessed this letter.") as true
        case Correct => putStr("Good job, $name, you guess correctly. Keep going!") as true
        case Incorrect => putStr("Sorry, that was incorrect. Try again!") as true
      }
      _ <- if (loop) gameLoop(state) else ZIO.unit
    } yield ()).refineToOrDie[IOException]

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      *  f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      *  Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won extends GuessResult
    case object Lost extends GuessResult
    case object Correct extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  def guessResult(oldState: State, newState: State, char: Char): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
    * EXERCISE 18
    *
    * Implement hangman using `Dictionary.Dictionary` for the words,
    * and the above helper functions.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      word <- chooseWord
      name <- getName
      state = State(name, Set(), word)
      _ <- renderState(state)
      _ <- gameLoop(state)
    } yield ()).fold(_ => 1, _ => 0)
}

/**
  * GRADUATION PROJECT
  *
  * Implement a game of tic tac toe using ZIO, then develop unit tests to
  * demonstrate its correctness and testability.
  */
object TicTacToe extends App {
  import zio.console._

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }
    final def render: String = renderChar.toString
  }
  object Mark {
    case object X extends Mark
    case object O extends Mark
  }

  final case class Board private (value: Vector[Vector[Option[Mark]]]) {

    /**
      * Retrieves the mark at the specified row/col.
      */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
      * Places a mark on the board at the specified row/col.
      */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3)
        Some(
          copy(value = value.updated(row, value(row).updated(col, Some(mark))))
        )
      else None

    /**
      * Renders the board to a string.
      */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
      * Returns which mark won the game, if any.
      */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int,
        mark: Mark
    ): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int
    ): Iterable[Option[Mark]] =
      for {
        row <- (row0 to (row0 + rowInc * 2))
        col <- (col0 to (col0 + colInc * 2))
      } yield value(row)(col)
  }
  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(
        first: Iterable[Char],
        second: Iterable[Char],
        third: Iterable[Char]
    ): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
            )
          )
        )
      }
  }

  val TestBoard = Board
    .fromChars(
      List(' ', 'O', 'X'),
      List('O', 'X', 'O'),
      List('X', ' ', ' ')
    )
    .get
    .render

  /**
    * The entry point to the game will be here.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn(TestBoard) as 0
}
