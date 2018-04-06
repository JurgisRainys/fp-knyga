package rainysjurgis

import scala.language.implicitConversions
import scala.util.matching.Regex

object chapter9 {
  trait Parsers[Parser[+_]] { self =>
    implicit def run[A](p: Parser[A])(input: String): Result[A]

    implicit def asParserOps[A, B](a: A)(implicit f: A => Parser[B]): ParserOps[B] = ParserOps(f(a))

    implicit def regex(r: Regex): Parser[String]

    implicit def string(s: String): Parser[String]

    implicit def char(c: Char): Parser[Char] = {
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

    def slice[A](p: Parser[A]): Parser[String]

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
      p.flatMap(a => unit(f(a)))
    }

    def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
      (p1 ** p2).map { case (a, b) => f(a, b) }
    }

    def many[A](p: Parser[A]): Parser[List[A]] = {
      p.map2(p.many)(_ :: _) or succeed(List())
    }

    def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(p.many)(_ :: _)

    def manyP1EndsWithP2[A](p1: Parser[A], p2: => Parser[A]): Parser[List[A]] = {
      p1.map2(p1.manyP1EndsWithP2(p2)) { case (a, list) => a :: list } or p2.map(List(_))
//      attempt(p1.map2(p1.manyP1EndsWithP2(p2)) { case (a, list) => a :: list }) or p2.map(List(_))
    }
    // [ 5, 6 ,2]

    def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
      for {
        a <- p1
        b <- p2
      } yield (a, b)
    }

    def manySeparatedBy[A](p: Parser[A])(separator: String): Parser[List[A]] = {
      import values._
      val trimmedSpaces = spacesOptional *! p !* spacesOptional
      (trimmedSpaces !* separator).manyP1EndsWithP2(trimmedSpaces)
    }
    def label[A](p: Parser[A])(msg: String): Parser[A]

    def attempt[A](p: Parser[A]): Parser[A]

    def !*[A, B](p1: Parser[A], p2: Parser[B]): Parser[A] = (p1 ** p2).map(_._1)// Discard is desines

    def !*+[A, B, C](p1: Parser[A], p2: Parser[(B, C)]): Parser[(A, C)] = {
      for {
        a <- p1
        bcTuple <- p2
      } yield (a, bcTuple._2)
    }

    def !+*[A, B, C](p1: Parser[A], p2: Parser[(B, C)]): Parser[(A, B)] = {
      for {
        a <- p1
        bcTuple <- p2
      } yield (a, bcTuple._1)
    }

    def *![A, B](p1: Parser[A], p2: Parser[B]): Parser[B] = (p1 ** p2).map(_._2)

    object values {
      val spaces: Parser[String] = regex("\\s".r).many.slice //istikro spaces, tabs, newlines etc.

      val spacesOptional: Parser[String] = regex("\\s".r).many.slice //istikro spaces, tabs, newlines etc.

      val digits: Parser[String] = regex("[0-9]+".r)

      val double: Parser[Double] = regex("((\\-|\\+)?)(0|([1-9][0-9]*))(\\.[0-9]+)?".r).map(s => s.toDouble)

      val ints: Parser[Int] = digits.map(_.toInt)

      val dot: Parser[String] = regex("\\.".r)

      val untilClosingBracket: Parser[String] = regex("[^\"]+.".r).map(str => str.substring(0, str.length - 1))

      val squareBracketOpen: Parser[String] = regex("\\[".r)

      val squareBracketClose: Parser[String] = regex("\\]".r)

      val curlyBracketOpen: Parser[String] = regex("\\{".r)

      val curlyBracketClose: Parser[String] = regex("\\}".r)

      val quotation: Parser[Char] = char('"')
    }

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = p | p2

      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)

      def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

      def many: Parser[List[A]] = self.many(p)

      def many1: Parser[List[A]] = self.many1(p)

      def manyP1EndsWithP2(p2: Parser[A]): Parser[List[A]] = self.manyP1EndsWithP2(p, p2)

      def slice: Parser[String] = self.slice(p)

      def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

      def **[B](p2: => Parser[B]): Parser[(A, B)] = p.product(p2)

      def !*[B](p2: Parser[B]): Parser[A] = self !* (p, p2)

      def !*+[B, C](p2: Parser[(B, C)]): Parser[(A, C)] = self !*+ (p, p2)

      def !+*[B, C](p2: Parser[(B, C)]): Parser[(A, B)] = self !+* (p, p2)

      def *![B](p2: Parser[B]): Parser[B] = self *! (p, p2)

      def manySeparatedBy(separator: String): Parser[List[A]] = self.manySeparatedBy(p)(separator)

      def label(msg: String): Parser[A] = self.label(p)(msg)

      def attempt: Parser[A] = self.attempt(p)

      def run(input: String): Result[A] = self.run(p)(input)
    }

    sealed trait JSON extends Product with Serializable

    object JSON {
      case object JNull extends JSON
      case class JNumber(get: Double) extends JSON
      case class JString(get: String) extends JSON
      case class JBool(get: Boolean) extends JSON
      case class JArray(get: IndexedSeq[JSON]) extends JSON
      case class JObject(get: Map[String, JSON]) extends JSON

      import values._

      lazy val parser: Parser[JSON] = {
        spacesOptional *! (jNullParser | jBoolParser | jNumberParser | jStringParser | jArrayParser | jObjectParser) !* spacesOptional
      }

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
        quotation *! untilClosingBracket.map(JString)
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

        (spacesOptional *! key !*+ betweenKeyValue ** value)
          .map { case (key, value) => (key.get, value) }
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

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def take(n: Int): String = {
      if (offset + n > input.length) input.substring(offset)
      else input.substring(offset, offset + n) + "..."
    }

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))

    def advanceBy(n: Int): Location =
      copy(offset = offset + n)
  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)

    def label[A](s: String): ParseError =
      ParseError(latestLoc.map((_, s)).toList)

    def latestLoc: Option[Location] =
      latest.map(_._1)

    def latest: Option[(Location,String)] =
      stack.lastOption

    override def toString =
      if (stack.isEmpty) "no error message"
      else {
        val collapsed = collapseStack(stack)
        val context =
          collapsed.lastOption.map("\n\n" + _._1.line).getOrElse("") +
            collapsed.lastOption.map("\n" + _._1.col).getOrElse("")
        collapsed.map {
          case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg
        }.mkString("\n") + context
      }

    def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
      s.groupBy(_._1).
        mapValues(_.map(_._2).mkString("; ")).
        toList.sortBy(_._1.offset)
    def formatLoc(l: Location): String = l.line + "." + l.col

  }

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, isCommitted, r) => Failure(f(e), isCommitted, r)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(err, commit, r) => Failure(err, commit || isCommitted, r)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true, r) => Failure(e, false, r)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }

    def parsedData: Option[A] = this match {
      case Success(get, _) => Some(get)
      case Failure(_, _, _) => None
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean, regToMatch: String) extends Result[Nothing]

  object MyParsers extends Parsers[Parser] {
    override implicit def regex(r: Regex): Parser[String] = (loc: Location) => {
      r.findPrefixOf(loc.input.substring(loc.offset)) match {
        case None => Failure(loc.toError("couldnt match regex: \"" + r.toString + "\""), false, r.toString)
        case Some(str) => Success(str, str.length)
      }
    }

    override implicit def string(s: String): Parser[String] = {
      label(s.r)("input didn't match pattern: \"" +  s + "\"")
    }

    override def slice[A](p: Parser[A]): Parser[String] = (loc: Location) => {
      val startOffset = loc.offset
      p(loc) match {
        case Success(_, charsConsumed) => Success(loc.input.substring(startOffset, startOffset + charsConsumed), charsConsumed)
        case fail @ Failure(_, _, _) => fail
      }
    }

    def scope[A](p: Parser[A])(msg: String): Parser[A] = {
      loc => p(loc).mapError(_.push(loc, msg))
    }

    def attempt[A](p: Parser[A]): Parser[A] =
      loc => p(loc).uncommit

    def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
      loc => x.attempt(loc) match {
        case Failure(x, false, _) => y(loc)
        case r => r
      }

    def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
      loc => f(loc) match {
        case Success(a, n) =>
          g(a)(loc.advanceBy(n))
          .addCommit(n != 0)
          .advanceSuccess(n)
        case err @ Failure(_, _, _) => err
      }

    override implicit def run[A](p: Parser[A])(input: String): Result[A] = {
      p(Location(input)) match {
        case result @ Failure(get, _, toMatch) =>
          get.stack.foreach { case (loc, msg) =>
            println(msg + "; inputs substring: \"" + loc.take(toMatch.length + 3) + "\"")
          }
          result
        case result @ Success(_, charsConsumed) =>
          if (input.length == charsConsumed) { println("String parsed successfully!"); result }
          else {
            val loc = Location(input.substring(charsConsumed))
            val msg = "Inputs substring left unmatched: \"" + loc.take(5) + "\""
            println("Parsed only a portion of the input successfully. Chars parsed: " + charsConsumed)
            println(msg)
            Failure(ParseError(List()).push(Location(input, charsConsumed), msg), true, "")
          }
      }
    }

    override def unit[A](a: A): Parser[A] = _ => Success(a, 0)

    override def label[A](p: Parser[A])(msg: String): Parser[A] = {
      loc => p(loc).mapError(_.label(msg))
    }
  }

  def test: Unit = {
    import MyParsers.values._
    import MyParsers._
    val exampleJSON = " { \"xd\": { \"xd2\": 0} } "
    val exampleJSON2 = " { \"xd\": [ 0, 1] } "
    val exampleJSONObj2 = " { \"xd\": 0.2, \"xd2\": \"lol\" } "

    val bigObj = "{  \"widget\": {\n    \"debug\": \"on\",\n    \"window\": {\n        \"title\": \"Sample Konfabulator Widget\",\n        \"name\": \"main_window\",\n        \"width\": 500,\n        \"height\": 500\n    },\n    \"image\": { \n        \"src\": \"Images/Sun.png\",\n        \"name\": \"sun1\",\n        \"hOffset\": 250,\n        \"vOffset\": 250,\n        \"alignment\": \"center\"\n    },\n    \"text\": {\n        \"data\": \"Click Here\",\n        \"size\": 36,\n        \"style\": \"bold\",\n        \"name\": \"text1\",\n        \"hOffset\": 250,\n        \"vOffset\": 100,\n        \"alignment\": \"center\",\n        \"onMouseUp\": \"sun1.opacity = (sun1.opacity / 100) * 90;\"\n    }\n}}   "
    val bigObj2 = "{\"menu\": {\n    \"header\": \"SVG Viewer\",\n    \"items\": [\n        {\"id\": \"Open\"},\n        {\"id\": \"OpenNew\", \"label\": \"Open New\"},\n        null,\n        {\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},\n        {\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},\n        {\"id\": \"OriginalView\", \"label\": \"Original View\"},\n        null,\n        {\"id\": \"Quality\"},\n        {\"id\": \"Pause\"},\n        {\"id\": \"Mute\"},\n        null,\n        {\"id\": \"Find\", \"label\": \"Find...\"},\n        {\"id\": \"FindAgain\", \"label\": \"Find Again\"},\n        {\"id\": \"Copy\"},\n        {\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},\n        {\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},\n        {\"id\": \"ViewSVG\", \"label\": \"View SVG\"},\n        {\"id\": \"ViewSource\", \"label\": \"View Source\"},\n        {\"id\": \"SaveAs\", \"label\": \"Save As\"},\n        null,\n        {\"id\": \"Help\"},\n        {\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}\n    ]\n}}"
    val bigObj3 = "{\n    \"glossary\": {\n        \"title\": \"example glossary\",\n\t\t\"GlossDiv\": {\n            \"title\": \"S\",\n\t\t\t\"GlossList\": {\n                \"GlossEntry\": {\n                    \"ID\": \"SGML\",\n\t\t\t\t\t\"SortAs\": \"SGML\",\n\t\t\t\t\t\"GlossTerm\": \"Standard Generalized Markup Language\",\n\t\t\t\t\t\"Acronym\": \"SGML\",\n\t\t\t\t\t\"Abbrev\": \"ISO 8879:1986\",\n\t\t\t\t\t\"GlossDef\": {\n                        \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",\n\t\t\t\t\t\t\"GlossSeeAlso\": [\"GML\", \"XML\"]\n                    },\n\t\t\t\t\t\"GlossSee\": \"markup\"\n                }\n            }\n        }\n    }\n}"
    val jnumber = "-0.5"
    val jnull = " null"
    val jstr = "\"adsdasd \""
    val jBool = "f5alse"
    val jArray = "[ 1.0, 2, 0.6, 14 ]"
    val jArray2 = "[ { \"x\": 2 }, {\"xd\": 5} ]"

    val s2 = "[]xd12   6  "
    val s = "[]xd1234567"
    val loc = Location(s.toString)

//    val test = MyParsers.run((squareBracketOpen | squareBracketClose) ** squareBracketClose ** "xd123")(s2)
    val test2 = MyParsers.run(JSON.parser)(bigObj3).parsedData
    println(test2)

    // "xd123 LOL"
    // (char('x') ** string("123")).slice == string("x123")
    // returnina ('x', "123") ir ("x123")
    // nesucceedins nes inputas nevisiskai atitinka patterna (ilgesnis " LOL" simboliais)
  }
}
