import fpinscala.parsing.JSON.{JArray, JBool, JNumber, JObject, JString}
import fpinscala.parsing.{JSON, Location, ParseError}

import scala.::
import scala.collection.IterableOnce.iterableOnceExtensionMethods

object ParsingDemo {

  val txt1 =
    """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""
  
  val jsonTxt2 =
    """
{
  "Active"  : true,
  "Company name" : "Microsoft Corporation",
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ],
  "Ticker"  : "MSFT",
  "Shares outstanding" : 8.38e9,
  "Price"   : 30.66
}
"""

  val jsonTxt3 =
    """
{
  "players": {
    "Rafael": [2,3,4],
    "Roger": [3,4,5]
  },
  "events": [
    {"Monte Carlo": "April", "Wimbledon": "July", "Roland Garros": "June"},
    {"Indian Wells": "March", "Australian Open": "January"}
  ]
}
"""

  val jsonTxt4 =
    """
{
  "events": [
    {"Monte Carlo": "April", "Wimbledon": "July", "Roland Garros": "June"},
    {"Indian Wells": "March", "Australian Open": "January"}
  ],
  "players": {
    "Roger": [3,4,5],
    "Rafael": [2,3,4]
  }
}
"""

  def go = {
    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val json: Parser[JSON] = JSON.jsonParser(P)
    val resultOfParsing1 = P.run(json)(txt1) // this parses JSON input into a JSON object
    val res1 = fpinscala.parsing.Reference.run(
      JSON.jsonParser(fpinscala.parsing.Reference)
    )(txt1)
    println("Parsing first text")
    resultOfParsing1.fold(println,println)
    println("Deserialize first text")
    resultOfParsing1.flatMap(j => unpack(j)).map(dto => println(dto)).map(_ => ())

    println("Second way")
    resultOfParsing1.flatMap(
      j => betterUnpackUsingForComprehension(j)).map(dto => println(dto)).map(_ => ())

    println("Third way")
    resultOfParsing1.flatMap(
      j => betterUnpackUsingFlatMap(j)).map(dto => println(dto)).map(_ => ())

    val resultOfParsing2 = P.run(json)(jsonTxt2)
    println("Parsing second text")
    resultOfParsing2.fold(println,println)
    println("Deserialize second text")
    resultOfParsing2.flatMap(j => unpack(j)).map(dto => println(dto)).map(_ => ())
    println("Second way")
    resultOfParsing2.flatMap(j => betterUnpackUsingForComprehension(j)).map(dto => println(dto)).map(_ => ())
    println("Third way")
    resultOfParsing2.flatMap(j => betterUnpackUsingFlatMap(j)).map(dto => println(dto)).map(_ => ())
    val resultOfParsing3 = P.run(json)(jsonTxt3)
    println("Parsing third text")
    resultOfParsing3.fold(println,println)
    val resultOfParsing4 = P.run(json)(jsonTxt4)
    println("Parsing fourth text")
    resultOfParsing4.fold(println,println)
  }

  case class SampleDTO(
      companyName: String,
      ticker: String,
      isActive: Boolean,
      price: Double,
      sharesOutstanding: Double,
      relatedCompanies: List[String])

  case class SampeDTO2(
      playerCategories: Map[String,List[Int]],
      eventCalendar: List[Map[String,String]])

  def unpack(json: JSON): Either[ParseError,SampleDTO] = {
    val res = json match {
      case jObject: JObject =>
        for {
          companyName <- jObject.get("Company name") match {
            case jString: JString => Right(jString.get)
            case _ => Left(ParseError(List((Location("Could not unpack companyName"),"companyName"))))
          }
          ticker <- jObject.get("Ticker") match {
            case jString: JString => Right(jString.get)
            case _ => Left(ParseError(List((Location("Could not unpack ticker"),"ticker"))))
          }
          isActive <- jObject.get("Active") match {
            case jBool: JBool => Right(jBool.get)
            case _ => Left(ParseError(List((Location("Could not unpack isActive"),"isActive"))))
          }
          price <- jObject.get("Price") match {
            case jNumber: JNumber => Right(jNumber.get)
            case _ => Left(ParseError(List((Location("Could not unpack price"),"price"))))
          }
          shares <- jObject.get("Shares outstanding") match {
            case jNumber: JNumber => Right(jNumber.get)
            case _ => Left(ParseError(List((Location("Could not unpack shares"),"shares"))))
          }
          relatedPacked <- jObject.get("Related companies") match {
            case jArray: JArray => Right(jArray.get)
            case _ => Left(ParseError(List((Location("Could not unpack related"),"related"))))
          }
          related <- unpackList(relatedPacked.toList, Right(List.empty))
        } yield SampleDTO(companyName,ticker,isActive,price,shares,related)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
    }
    res
  }

  def unpackList(c: List[JSON], r: Either[ParseError,List[String]]): Either[ParseError,List[String]] =
    c match {
      case ::(head, next) => head match {
        case JString(v) => unpackList(next, r.flatMap(list => Right(v :: list)))
        case p: ParseError => Left(p)
      }
      case Nil => r
    }

  def betterUnpackUsingForComprehension(json: JSON): Either[ParseError,SampleDTO] =
    json match {
      case jObject: JObject =>
        for {
          companyName <- unpackString(jObject, "Company name")
          ticker <- unpackString(jObject, "Ticker")
          isActive <- unpackBoolean(jObject, "Active")
          price <- unpackNumber(jObject, "Price")
          shares <- unpackNumber(jObject, "Shares outstanding")
          related <- unpackArray(jObject, "Related companies")
        } yield SampleDTO(companyName, ticker, isActive, price, shares, related)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
    }

  def betterUnpackUsingFlatMap(json: JSON): Either[ParseError, SampleDTO] = {
    json match {
      case jObject: JObject =>
        unpackString(jObject, "Company name")
          .flatMap(companyName => unpackString(jObject, "Ticker")
          .flatMap(ticker => unpackBoolean(jObject, "Active")
          .flatMap(isActive => unpackNumber(jObject, "Price")
          .flatMap(price => unpackNumber(jObject, "Shares outstanding")
          .flatMap(shares => unpackArray(jObject, "Related companies")
          .map(related => SampleDTO(companyName, ticker, isActive, price, shares, related)))))))
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
    }
  }

  def unpackString(jObject: JObject, key: String): Either[ParseError,String] = jObject.get(key) match {
    case jString: JString => Right(jString.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackBoolean(jObject: JObject, key: String): Either[ParseError, Boolean] = jObject.get(key) match {
    case jBool: JBool => Right(jBool.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackNumber(jObject: JObject, key: String): Either[ParseError, Double] = jObject.get(key) match {
    case jNumber: JNumber => Right(jNumber.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackArray(jObject: JObject, key: String): Either[ParseError, List[String]] = {
    for {
      relatedPacked <- jObject.get(key) match {
        case jArray: JArray => Right(jArray.get)
        case _ => Left(ParseError(List((Location("Could not unpack related"), "related"))))
      }
      related <- unpackList(relatedPacked.toList, Right(List.empty))
    } yield related
  }

}
