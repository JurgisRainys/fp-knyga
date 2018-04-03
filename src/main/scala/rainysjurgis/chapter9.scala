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

    def unit[A](a: A): Parser[A]

    def succeed[A](a: A): Parser[A] = unit(a)

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
    // "xd123 LOL"
    // (char('x') ** string("123")).slice == string("x123")
    // returnina ('x', "123") ir ("x123")

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = p | p2

      def flatMap[B](f: A => Parser[B]): Parser[B] = ???

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

      def slice: Parser[String] = ???

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
          (_, c) <- p2
        } yield (a, c)
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

      def scope[A](msg: String): Parser[A] = ???

      def attempt[A](p: Parser[A]): Parser[A] = ???
    }

    object Laws {
      val stringList: SGen[List[String]] = SGen.listOf(Gen.alphabeticString(10))

      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        Prop.forAll(in.unsized)(str => run(p1)(str) == run(p2)(str))

      def stringTest(f: String => Boolean): Prop = {
        Prop.forAll(stringList)(list => list.count(f) == list.size)
      }

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(identity))(in)

      def unitLaw[A](a: A): Prop =
        stringTest(str => unit(a)(str) == Right(a))

      def manyLaw: Prop =
        stringTest(str => char('a').many.slice.map(_.length)(str) == Right(3))

      def many1Law: Prop =
        stringTest(str => char('0').many1(str) match {
          case Left(_) => true
          case _ => false
        })

      def productLaw: Prop = {
        Prop.asProp {
          val p = char('a')
          (p ** p ** p)("aaa") == p.many("aaa")
        }
      }

      def listOfNLaw: Prop = {
        Prop.asProp(listOfN(3, "ab" | "cad")("ababcad") == Right("ababcad"))
        Prop.asProp(listOfN(3, "ab" | "cad")("cadabab") == Right("cadabab"))
        Prop.asProp(listOfN(3, "ab" | "cad")("ababab") == Right("ababab"))
      }

      def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
        Prop.forAll(inputs ** Gen.alphabeticString(10).unsized) { case (input, msg) =>
          run(p.label(msg))(input) match {
            case Left(e) => errorMessage(e) == msg
            case _ => true
          }
        }
    }

    sealed trait JSON

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
      }
    }
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))
  }

  def errorLocation(e: ParseError): Location = ???
  def errorMessage(e: ParseError): String = ???

  case class ParseError(stack: List[(Location, String)])

  type Parser[+A] = Location => Result[A]
  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  def test: Unit = {
    val exampleJSON = " { \"xd\": \"xD\" } "
  }
}
