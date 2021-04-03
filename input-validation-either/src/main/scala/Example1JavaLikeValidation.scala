import com.kafecito.model.Email

object Example1JavaLikeValidation {

  def main(args: Array[String]): Unit =
    val goodEmail: String = "mrme@xmail.com"
    val goodSSN: String = "111-11-2345"
    val goodAge: String = "49"

    validateEmail(goodEmail)
    validateSSN(goodSSN)
    validateAge(goodAge)

    val goodResult: String = f"email: ${goodEmail}, ssn: ${goodSSN}, age $goodAge}"

    val badAge = "old"
    val badSSN = "abc-22-2212"

    validateAge(badAge)
    validateSSN(badSSN)

    val badResult: String = f"email: ${goodEmail}, ssn: ${badSSN}, age $badAge}"

    println(badResult)
    println(goodResult)
  

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

}
