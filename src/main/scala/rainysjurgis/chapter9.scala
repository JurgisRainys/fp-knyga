package rainysjurgis

import rainysjurgis.chapter8._

import scala.util.matching.Regex

object chapter9 {

  trait Parsers[Parser[+_]] { self =>
    implicit def run[A](p: Parser[A])(input: String): Result[A]

    implicit def asParserOps[A, B](a: A)(implicit f: A => Parser[B]): ParserOps[B] = ParserOps(f(a))

    implicit def regex(r: Regex): Parser[String]

    implicit def string(s: String): Parser[String]

    def char(c: Char): Parser[Char] = {
      string(c.toString).map(_.charAt(0))
    }

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def lazyF[A, B](f: => B): B = f

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
      p.map(a => List.fill(n)(a))

//      su map2 ir succeed
//      if (n <= 0) succeed(List())
//      else map2(p, listOfN(n-1, p))(_ :: _)
    }

    def unit[A](a: A): Parser[A] = string("") map (_ => a)

    def succeed[A](a: A): Parser[A] = unit(a)

    def slice[A](p: Parser[A]): Parser[String] = ???

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    object values {
      val spaces: Parser[String] = regex("\\s".r).many.slice //istikro spaces, tabs, newlines etc.

      val spacesOptional: Parser[String] = regex("\\s".r).many.slice //istikro spaces, tabs, newlines etc.

      val digits: Parser[String] = regex("[0-9]+".r)

      val double: Parser[Double] = regex("(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?".r).map(s => s.toDouble)

      val ints: Parser[Int] = digits.map(_.toInt)

      val dot: Parser[String] = regex("\\.".r)

      //    val anything: Parser[String] = regex(".+".r)

      val untilClosingBracket: Parser[String] = regex("[^\"]+." r)

      val squareBracketOpen: Parser[Char] = char('[')

      val squareBracketClose: Parser[Char] = char(']')

      val curlyBracketOpen: Parser[Char] = char('{')

      val curlyBracketClose: Parser[Char] = char('}')

      val quotation: Parser[Char] = char('"')
    }

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = p | p2

      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

      def map[B](f: A => B): Parser[B] = {
        p.flatMap(a => unit(f(a)))
      }

      def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
        (p ** p2).map { case (a, b) => f(a, b) }
      }

      def many: Parser[List[A]] =
        p.manyUsingMap2

      def manyUsingMap2: Parser[List[A]] = {
        p.map2(p.manyUsingMap2)(_ :: _) or succeed(List())
        //p.map2(lazyF(p.manyUsingMap2))(_ :: _) or unit(List.empty) // jei map2 antras argumentas ne lazy
      }

      def many1: Parser[List[A]] = p.map2(p.many)(_ :: _)

      def slice: Parser[String] = self.slice(p)

      def product[B](p2: => Parser[B]): Parser[(A, B)] = {
        for {
          a <- p
          b <- p2
        } yield (a, b)
      }

      def **[B](p2: Parser[B]): Parser[(A, B)] = p.product(p2)

      def !*[B](p2: Parser[B]): Parser[A] = (p ** p2).map(_._1)// jauciu blogai. Discard is desines

      // jauciu is desines pradeda konstruot sitie operatoriai
      // tai kad: a !* b ** c nudiscardintu tik b, bet ne c, operatorius !* netinka
//      def !**[B, C](p2: Parser[(B, _*)]): Parser[(A, _*)] = {
      def !**[B, C](p2: Parser[(B, C)]): Parser[(A, C)] = {
        for {
          a <- p
          bc <- p2
        } yield (a, bc._2)
      }

//      def *![B](p2: Parser[B]): Parser[B] = (p ** p2).flatMap(_ => p2) // jauciu blogai. Discard is kaires
      def *![B](p2: Parser[B]): Parser[B] = (p ** p2).map(_._2)// jauciu blogai. Discard is kaires

      def digitFollowedByChar: Parser[(String, List[A])] = {
        "[0-9]+".r.flatMap(num => listOfN(num.toInt, p).map((num, _)))
      }

      def optional = p | unit()

      def manySeparatedBy(separator: String): Parser[List[A]] = {
        import values._
        val trimmedSpaces = spacesOptional *! p !* spacesOptional
        (trimmedSpaces !* separator).many.map2(trimmedSpaces)(_ :+ _)
      }

      def label[A](msg: String): Parser[A] = ???

      def attempt[A](p: Parser[A]): Parser[A] = ???
    }
//    object Laws {
//      val stringList: SGen[List[String]] = SGen.listOf(Gen.alphabeticString(10))
//
//      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
//        Prop.forAll(in.unsized)(str => run(p1)(str) == run(p2)(str))
//
//      def stringTest(f: String => Boolean): Prop = {
//        Prop.forAll(stringList)(list => list.count(f) == list.size)
//      }
//
//      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
//        equal(p, p.map(identity))(in)
//
//      def unitLaw[A](a: A): Prop =
//        stringTest(str => unit(a)(str) match {
//          case Success(a, _) => true
//          case _ => false
//        })
//
//      def manyLaw: Prop =
//        stringTest(str => char('a').many.slice.map(_.length)(str) match {
//          case Success(3, _) => true
//          case _ => false
//        })
//
//      def many1Law: Prop =
//        stringTest(str => char('0').many1(str) match {
//          case Failure(_, _) => true
//          case _ => false
//        })
//
//      def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
//        Prop.forAll(inputs ** Gen.alphabeticString(10).unsized) { case (input, msg) =>
//          run(p.label(msg))(input) match {
//            case Failure(e, _) => errorMessage(e) == msg
//            case _ => true
//          }
//        }
//    }
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))

    def advanceBy(n: Int): Location =
      copy(offset = offset + n)
  }

//  def errorLocation(e: ParseError): Location = ???
//  def errorMessage(e: ParseError): String = ???

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)

    def label[A](s: String): ParseError =
      ParseError(latestLoc.map((_, s)).toList)

    def latestLoc: Option[Location] =
      latest.map(_._1)

    def latest: Option[(Location,String)] =
      stack.lastOption
  }

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(err, commit) => Failure(err, commit || isCommitted)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  object MyParsers extends Parsers[Parser] {
    override implicit def regex(r: Regex): Parser[String] = (loc: Location) => {
      r.findPrefixOf(loc.input.substring(loc.offset)) match {
        case None => Failure(loc.toError("couldnt match regex: " + r.toString), false)
        case Some(str) => Success(str, str.length)
      }
    }

    override implicit def string(s: String): Parser[String] = {
      label("couldnt find string: " +  s)(s.r)
    }

    override def slice[A](p: Parser[A]): Parser[String] = (loc: Location) => {
      val startOffset = loc.offset
      p(loc) match {
        case Success(_, charsConsumed) => Success(loc.input.substring(startOffset, startOffset + charsConsumed), charsConsumed)
        case fail @ Failure(_, _) => fail
      }
    }

    def scope[A](msg: String)(p: Parser[A]): Parser[A] = {
      loc => p(loc).mapError(_.push(loc, msg))
    }

    def label[A](msg: String)(p: Parser[A]): Parser[A] =
      loc => p(loc).mapError(_.label(msg))

    def attempt[A](p: Parser[A]): Parser[A] =
      loc => p(loc).uncommit

    def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
      loc => x(loc) match {
        case Failure(_, false) => y(loc)
        case r => r
      }

    def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
      loc => f(loc) match {
        case Success(a, n) => g(a)(loc.advanceBy(n))
          .addCommit(n != 0)
          .advanceSuccess(n)
        case err @ Failure(_,_) => err
      }

    override implicit def run[A](p: Parser[A])(input: String): Result[A] = ???

    sealed trait JSON extends Product with Serializable

    object JSON {
      case object JNull extends JSON
      case class JNumber(get: Double) extends JSON
      case class JString(get: String) extends JSON
      case class JBool(get: Boolean) extends JSON
      case class JArray(get: IndexedSeq[JSON]) extends JSON
      case class JObject(get: Map[String, JSON]) extends JSON

      lazy val parser: Parser[JSON] = {
        import values._

        val jNullParser: Parser[JNull.type] = string("null").map(_ => JNull)

        val jBoolParser: Parser[JBool] = {
          string("false").map(_ => JBool(false)) | string("true").map(_ => JBool(true))
        }

        val jNumberParser: Parser[JNumber] = {
          double.map(JNumber)
          //        val jNumberParser: Parser[JNumber] = {
          //          // .5
          //          // -.5
          //          // 0.5
          //          // 1
          //          // 1.2
          //          // +.2
          //          // s.toDouble
          //          def sign: Parser[Char] = char('+') | char('-')
          //
          //          def addSign[A](optionalSign: A)(num: Int): Option[Int] = optionalSign match {
          //            case '-' => Some(-num)
          //            case '+' | () => Some(num)
          //            case _ => None
          //          }
          //
          //          def addDigitsAfterDot[A](numBeforeDot: Int, digitsAfterDot: A): Option[Double] = digitsAfterDot match {
          //            case () => Some(numBeforeDot)
          //            case _ => None
          //            case (_, i: Int) => Some((numBeforeDot + "." + i).toDouble)
          //          }
          //
          //          def ordinaryNumRepresentation: Parser[Double] = {
          //            //sign.optional ** digits ** (dot ** digits).optional
          //            for {
          //              optionalSign <- sign.optional
          //              digitsBeforeDot <- ints
          //              optionalDigitsAfterDot <- (dot ** ints).optional
          //            } yield {
          //              addSign(optionalSign)(digitsBeforeDot)
          //                .flatMap(num => addDigitsAfterDot(num, optionalDigitsAfterDot))
          //                .getOrElse(0)
          //            }
          //          }
          //
          //          def numStartingWithDot: Parser[Double] = {
          //            //| sign.optional ** dot ** digits
          //            for {
          //              optionalSign <- sign.optional
          //              _ <- dot
          //              numAfterDot <- ints
          //            } yield {
          //              addSign(optionalSign)(0)
          //                .flatMap(num => addDigitsAfterDot(num, numAfterDot))
          //                .getOrElse(0)
          //            }
          //          }
          //
          //          (ordinaryNumRepresentation | numStartingWithDot).map(JNumber)
          //        }
        }

        val jStringParser: Parser[JString] = {  // ar antra quotation aptiks, jei untilClosingBracket iki jo isparsina, ji iskaitant
          quotation *! untilClosingBracket.map(JString) !* quotation
        }

        val jArrayElemParser: Parser[JSON] = {
          spacesOptional *! parser !* spacesOptional
        }

        val jArrayParser: Parser[JArray] = {
          (
            squareBracketOpen
              *! spacesOptional
              *! jArrayElemParser.manySeparatedBy(",")
              !* spacesOptional
              !* squareBracketClose
            )
            .map(list => JArray(list.toIndexedSeq))
        }

        val jObjectElemParser: Parser[(String, JSON)] = {
          val key = jStringParser
          val betweenKeyValue = spacesOptional ** char(':') ** spacesOptional
          val value = parser

          // ** grazina (a, b), bet key !* (a, b) == key
          // reikia, kad grazintu (key, b)
          (spacesOptional *! key !** betweenKeyValue ** value) //netaip !* veikia
            .map { case (key, value) => (key.get, value) }

          // sitas (JString, Json) grazina
          //        spacesOptional *! key !** betweenKeyValue ** value //netaip !* veikia
        }

        val jObjectParser: Parser[JObject] = { // nuo spaces patikrint
          (
            curlyBracketOpen
              *! spacesOptional
              *! jObjectElemParser.manySeparatedBy(",")
              !* curlyBracketClose
            )
            .map(list => JObject(Map(list: _*)))
        }

        jObjectParser | jArrayParser | jNullParser | jBoolParser | jNumberParser | jStringParser
      }
    }
  }

  def test: Unit = {
    val exampleJSON = Location(" { \"xd\": \"xD\" } ")

    val x = MyParsers.JSON.parser(exampleJSON)
    println(x)
    // "xd123 LOL"
    // (char('x') ** string("123")).slice == string("x123")
    // returnina ('x', "123") ir ("x123")
    // nesucceedins nes inputas nevisiskai atitinka patterna (ilgesnis " LOL" simboliais)
  }
}
