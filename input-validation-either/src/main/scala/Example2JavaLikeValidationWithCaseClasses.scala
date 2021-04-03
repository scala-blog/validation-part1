
object Example2JavaLikeValidationWithCaseClasses:

  case class SSN(area: Int, group: Int, serial: Int)

  case class Email(user: String, domain: String)

  case class Age(age: Int)

  def main(args: Array[String]): Unit =

    val goodEmail: String = "mrme@xmail.com"
    val goodSSN: String = "111-11-2345"
    val goodAge: String = "49"

    validateEmail(goodEmail)
    validateSSN(goodSSN)
    validateAge(goodAge)

    val splitEmail: Array[String] = goodEmail.split("@")
    val emailUser = splitEmail(0)
    val emailDomain = splitEmail(1)
    val email: Email = Email(emailUser, emailDomain)

    val ageInt = goodAge.toInt
    val age: Age = Age(ageInt)

    val ssnSplit = goodSSN.split("-")
    val ssnArea = ssnSplit(0).toInt
    val sshGroup = ssnSplit(1).toInt
    val sshSerial = ssnSplit(2).toInt
    val ssn: SSN = SSN(ssnArea, sshGroup, sshSerial)

    val goodResult: String = f"email: ${email.user}@${email.domain}, ssn: ${ssn.area}-${ssn.group}-${ssn.serial}, age ${age.age}"
    println(goodResult)

  // I emitted the error cases due to the added boilerplate needed for this example to avoid too much information


  def validateEmail(string: String): Unit =
    if (string == null)
      throw new Error("Email is null")
    else
      val split = string.split("@")
      if (split.size != 2)
        throw new Error(s"Email ${string} is malformed")
      else ()


  def validateSSN(string: String): Unit =
    if (string == null)
      throw new Error("SSN is null")
    else
      val split = string.split("-")
      if (split.size != 3)
        throw new Error(s"Three different sets of digits expected but ${split.size} found")
      else if (split(0).filter(_.isDigit).size == 0)
        throw new Error(s"No digits found in area position ${string}")
      else if (split(1).filter(_.isDigit).size == 0)
        throw new Error(s"No digits found in group position ${string}")
      else if (split(2).filter(_.isDigit).size == 0)
        throw new Error(s"No digits found in serial position ${string}")
      else if (split(0).filter(!_.isDigit).size != 0)
        throw new Error(s"Invalid digit found in area  position ${string}")
      else if (split(1).filter(!_.isDigit).size != 0)
        throw new Error(s"Invalid digit found in group position ${string}")
      else if (split(2).filter(!_.isDigit).size != 0)
        throw new Error(s"Invalid digit found in serial position ${string}")
      else ()


  def validateAge(string: String): Unit =
    if (string == null)
      throw new Error("Email is null")
    else if (string.size == 0)
      throw new Error("Email is empty")
    else if (string.size != string.filter(_.isDigit).size)
      throw new Error(s"Email '$string' is malformed")
    else () 
      
