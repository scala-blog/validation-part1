package com.kafecito

import com.kafecito.model.SSN.fromStringCanThrowError

import scala.util.{Failure, Success, Try}

object model:

  sealed trait Error:
    def message: String = ""

    def code: Int // may help serialize too much info perhaps?

  // This one is better suited for monadic validation where you wont be able to accumulate errors
  type EItherWithError[A] = Either[Error, A]

  // This one is better suited for validation using applicatives where you can accumulate errors and validate all fields
  type EItherWithErrorList[A] = Either[Seq[Error], A]

  object EItherWithErrorList {
    def oneError[A](error: Error): EItherWithErrorList[A] =
      Left(List(error))
  }

  import EItherWithErrorList.oneError

    case class EmailError(override val message: String) extends Error :
    def code = 1

  case class SSNError(override val message: String) extends Error :
    def code = 3

  case class AgeError(override val message: String) extends Error :
    def code = 4

  case class SSN private(area: Int, group: Int, serial: Int)

  case class Email private(user: String, domain: String)

  case class Age private(age: Int)  

  object Age:
    def fromString(string: String): EItherWithErrorList[Age] =
      if (string == null)
        oneError(AgeError("Age is null"))
      else if(string.filter(_.isDigit).size == string.size && string.size != 0)
        Right(Age(string.toInt))
      else
        oneError(AgeError(s"Age is malfored : $string"))
        
  object SSN:
    def fromString(string: String): EItherWithErrorList[SSN] =
      Try {
        fromStringCanThrowError(string)
      } match
        case Success(either) => either
        case Failure(e) => Left(List(SSNError(e.getClass.getCanonicalName + s": Error parsing ssh ${string}" + e.getMessage)))


    private def fromStringCanThrowError(string: String): EItherWithErrorList[SSN] =
      if (string == null)
        oneError(SSNError("Social security is null"))
      else
        val split = string.split("-")
        if (split.size != 3)
          oneError(SSNError(s"Three different sets of digits expected but ${split.size} found"))
        else if (split(0).filter(_.isDigit).size == 0)
          oneError(SSNError(s"No digits found in area position ${string}"))
        else if (split(1).filter(_.isDigit).size == 0)
          oneError(SSNError(s"No digits found in group position ${string}"))
        else if (split(2).filter(_.isDigit).size == 0)
          oneError(SSNError(s"No digits found in serial position ${string}"))
        else if (split(0).filter(!_.isDigit).size != 0)
          oneError(SSNError(s"Invalid digit found in area  position ${string}"))
        else if (split(1).filter(!_.isDigit).size != 0)
          oneError(SSNError(s"Invalid digit found in group position ${string}"))
        else if (split(2).filter(!_.isDigit).size != 0)
          oneError(SSNError(s"Invalid digit found in serial position ${string}"))
        else
          Right(SSN(area = split(0).toInt, group = split(1).toInt, serial = split(2).toInt))


  object Email:
    def fromString(string: String): EItherWithErrorList[Email] =
      Try {
        fromStringCanThrow(string)
      } match
        case Success(either) => either
        case Failure(e) => Left(List(SSNError(e.getClass.getCanonicalName + s": Error parsing email ${string}" + e.getMessage)))

    private def fromStringCanThrow(string: String): EItherWithErrorList[Email] =
      if (string == null)
        oneError(EmailError("Email is null"))
      else
        val split = string.split("@")
        if (split.size != 2)
          oneError(EmailError(s"Email ${string} is malformed"))
        else
          Right(Email(user = split(0), domain = split(1)))

  



