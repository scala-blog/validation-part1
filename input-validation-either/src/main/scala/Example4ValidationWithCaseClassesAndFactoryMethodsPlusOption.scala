
object Example4ValidationWithCaseClassesAndFactoryMethodsPlusOption:

  case class SSN private(area: Int, group: Int, serial: Int)

  object SSN:
    def fromString(string: String): Option[SSN] =
      if (string == null)
        None
      else
        val split = string.split("-")
        if (split.size != 3)
          None
        else if (split(0).filter(_.isDigit).size == 0)
          None
        else if (split(1).filter(_.isDigit).size == 0)
          None
        else if (split(2).filter(_.isDigit).size == 0)
          None
        else if (split(0).filter(!_.isDigit).size != 0)
          None
        else if (split(1).filter(!_.isDigit).size != 0)
          None
        else if (split(2).filter(!_.isDigit).size != 0)
          None
        else
          Some(SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt))

  case class Email private(user: String, domain: String)

  object Email:
    def fromString(string: String): Option[Email] =
      if (string == null)
        None
      else
        val split = string.split("@")
        if (split.size != 2)
          None
        else
          Some(Email(user = split(0), domain = split(1)))


  case class Age private(age: Int)

  object Age:
    def fromString(string: String): Option[Age] =
      if (string == null)
        None
      else if (string.size == 0)
        None
      else if (string.size != string.filter(_.isDigit).size)
        None
      else
        Some(Age(string.toInt))

  def main(args: Array[String]): Unit =
    val goodEmail: String = "mrme@xmail.com"
    val goodSSN: String = "111-11-2345"
    val goodAge: String = "49"


    val goodResult: Option[String] = for
    email <- Email.fromString(goodEmail)
    age <- Age.fromString(goodAge)
    ssn <- SSN.fromString(goodSSN)
      yield f"email: ${email.user}@${email.domain}, ssn: ${ssn.area}-${ssn.group}-${ssn.serial}, age ${age.age}"

    val badAge: String = "old"
    val badSSN: String = "abc-22-2212"

    val badResult: Option[String] = for
    email <- Email.fromString(goodEmail)
    age <- Age.fromString(badAge)
    ssn <- SSN.fromString(badSSN)
      yield f"email: ${email.user}@${email.domain}, ssn: ${ssn.area}-${ssn.group}-${ssn.serial}, age ${age.age}"

    badResult match
      case Some(good: String) => println(good)
      case None => println("I know something bad happened. Nothing else.") // this will print out


    goodResult match
      case Some(good: String) => println(good) // this will print out
      case None => println("I know something bad happened. Nothing else.")
    
  

