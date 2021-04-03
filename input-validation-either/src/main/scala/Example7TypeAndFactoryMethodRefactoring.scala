import scala.io.Source
import scala.util.{Failure, Success, Try}

object Example7TypeAndFactoryMethodRefactoring:

  type EitherWithErrorString[A] = Either[String, A]

  trait SafeHandlerFromString[A] {
    def fromString(string: String): EitherWithErrorString[A] = {
      Try {
        fromStringUnsafe(string)
      } match
        case Success(good) => good
        case Failure(exception) => Left(s"ERROR: Unexpected: parsing $string: " + exception.getMessage)
    }

    protected def fromStringUnsafe(string: String): EitherWithErrorString[A] = Left("unimplemented")
  }

  case class SSN private(area: Int, group: Int, serial: Int)

  object SSN extends SafeHandlerFromString[SSN] :
    protected override def fromStringUnsafe(string: String): EitherWithErrorString[SSN] =
      if (string == null)
        Left("Social security is null")
      else
        val split = string.split("-")
        if (split.size != 3)
          Left(s"Three different sets of digits expected but ${split.size} found")
        else if (split(0).filter(_.isDigit).size == 0)
          Left(s"No digits found in area position '${string}'")
        else if (split(1).filter(_.isDigit).size == 0)
          Left(s"No digits found in group position '${string}'")
        else if (split(2).filter(_.isDigit).size == 0)
          Left(s"No digits found in serial position '${string}'")
        else if (split(0).filter(!_.isDigit).size != 0)
          Left(s"Invalid digit found in area  position '${string}'")
        else if (split(1).filter(!_.isDigit).size != 0)
          Left(s"Invalid digit found in group position '${string}'")
        else if (split(2).filter(!_.isDigit).size != 0)
          Left(s"Invalid digit found in serial position '${string}'")
        else
          Right(SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt))

  case class Email private(user: String, domain: String)

  object Email extends SafeHandlerFromString[Email] :
    protected override def fromStringUnsafe(string: String): EitherWithErrorString[Email] =
      if (string == null)
        Left("Email is null")
      else
        val split = string.split("@")
        if (split.size != 2)
          Left(s"Email is malformed. Trying to parse '$string''")
        else
          Right(Email(user = split(0), domain = split(1)))

  case class Age private(age: Int)

  object Age extends SafeHandlerFromString[Age] :
    protected override def fromStringUnsafe(string: String): EitherWithErrorString[Age] =
      if (string == null)
        Left("Age is null")
      else if (string.size == 0)
        Left("Age is empty")
      else if (string.size != string.filter(_.isDigit).size)
        Left(s"Age is malformed. trying to parse '$string''")
      else
        Right(Age(string.toInt))

  def main(args: Array[String]): Unit =
    val goodEmail: String = "mrme@xmail.com"
    val goodSSN: String = "111-11-2345"
    val goodAge: String = "49"

    val goodResult: EitherWithErrorString[String] = for {
      email <- Email.fromString(goodEmail)
      age <- Age.fromString(goodAge)
      ssn <- SSN.fromString(goodSSN)
    } yield f"email: ${email.user}@${email.domain}, ssn: ${ssn.area}-${ssn.group}-${ssn.serial}, age ${age.age}"

    val badAge = "old"
    val badSSN = "abc-22-2212"

    val badSsnEither: EitherWithErrorString[SSN] = SSN.fromString(badSSN)

    val badResult = for {
      email <- Email.fromString(goodEmail)
      age <- Age.fromString(badAge)
      ssn <- badSsnEither
    } yield f"email: ${email.user}@${email.domain}, ssn: ${ssn.area}-${ssn.group}-${ssn.serial}, age ${age.age}"

    badResult match
      case Right(good) => println(good)
      case Left(e) => println(s"I know at least one bad thing had happened. And I know what it is: ${e}") // this will print out


    goodResult match
      case Right(good) => println(good) // this will print out
      case Left(e) => println(s"I know at least one bad thing bad happened. And I know what it is: ${e}")

    badSsnEither match
      case Right(good) => println(good)
      case Left(f) => println(s"BAD SSN TRACKER: ERROR: ${f}")
      
