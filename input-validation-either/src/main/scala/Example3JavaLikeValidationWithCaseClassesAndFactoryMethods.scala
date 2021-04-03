
object Example3JavaLikeValidationWithCaseClassesAndFactoryMethods:

  case class SSN private(area: Int, group: Int, serial: Int)

  object SSN:
    def fromString(string: String): SSN =
      if (string == null)
        throw new Error("Email is null")
      else
        val split = string.split("-")
        if (split.size != 3)
          throw new Error(s"Three different sets of digits expected but ${split.size} found")
        else if (split(0).filter(_.isDigit).size == 0)
          throw new Error(s"No digits found in area position '${string}'")
        else if (split(1).filter(_.isDigit).size == 0)
          throw new Error(s"No digits found in group position '${string}'")
        else if (split(2).filter(_.isDigit).size == 0)
          throw new Error(s"No digits found in serial position '${string}'")
        else if (split(0).filter(!_.isDigit).size != 0)
          throw new Error(s"Invalid digit found in area  position '${string}'")
        else if (split(1).filter(!_.isDigit).size != 0)
          throw new Error(s"Invalid digit found in group position '${string}'")
        else if (split(2).filter(!_.isDigit).size != 0)
          throw new Error(s"Invalid digit found in serial position'${string}'")
        else
          SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt)

  case class Email private(user: String, domain: String)

  object Email:
    def fromString(string: String): Email =
      if (string == null)
        throw new Error("Email is null")
      else
        val split = string.split("@")
        if (split.size != 2)
          throw new Error(s"Email '${string}' is malformed")
        else
          Email(user = split(0), domain = split(1))


  case class Age private(age: Int)

  object Age:
    def fromString(string: String): Age =
      if (string == null)
        throw new Error("Age is null")
      else if (string.size == 0)
        throw new Error("Age is empty")
      else if (string.size != string.filter(_.isDigit).size)
        throw new Error(s"Age '${string}' is malformed")
      else
        Age(string.toInt)

  def main(args: Array[String]): Unit =
    val goodEmail: String = "mrme@xmail.com"
    val goodSSN: String = "111-11-2345"
    val goodAge: String = "49"

    val email: Email = Email.fromString(goodEmail)
    val age: Age = Age.fromString(goodAge)
    val ssn: SSN = SSN.fromString(goodSSN)

    val goodResult: String = f"email: ${email.user}@${email.domain}, ssn: ${ssn.area}-${ssn.group}-${ssn.serial}, age ${age.age}"

    val badAge: String = "old"
    val badSSN: String = "abc-22-2212"

    val ageBroken: Age = Age.fromString(badAge)
    val ssnBroken: SSN = SSN.fromString(badSSN) // We will never make it to here and we will need spaghetti code to handle this case

    // never happens due to previous exceptions
    val badResult: String = f"email: ${email.user}@${email.domain}, ssn: ${ssnBroken.area}-${ssnBroken.group}-${ssnBroken.serial}, age ${ageBroken.age}"


    println(badResult)

    println(goodResult)

