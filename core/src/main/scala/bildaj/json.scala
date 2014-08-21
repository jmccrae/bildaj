package bildaj

import com.fasterxml.jackson.core._
import com.fasterxml.jackson.core.JsonToken._

sealed trait JsonElement

case class JsonArray(list : List[JsonElement]) extends JsonElement {
  def ::(head : JsonElement) = JsonArray(head :: list)
}

case class JsonObject(obj : Map[String, JsonElement]) extends JsonElement {
  def ::(elem : (String, JsonElement)) = JsonObject(obj + elem)
}

case class JsonString(value : String) extends JsonElement

case class JsonFloat(value : Double) extends JsonElement

case class JsonInt(value : Int) extends JsonElement

case class JsonBool(value : Boolean) extends JsonElement

object JsonNull extends JsonElement {
  override def toString = "null"
}


object JsonUtils {
  def read(jsonParser : JsonParser) : JsonElement = jsonParser.nextToken() match {
    case END_ARRAY => throw new JsonReadingException("Out of place read")
    case END_OBJECT => throw new JsonReadingException("Out of place read")
    case FIELD_NAME => throw new JsonReadingException("Out of place read")
    case NOT_AVAILABLE => throw new JsonReadingException("Not available")
    case START_ARRAY => readArray(jsonParser)
    case START_OBJECT => readObject(jsonParser)
    case VALUE_EMBEDDED_OBJECT => throw new JsonReadingException("Embedded object")
    case VALUE_FALSE => JsonBool(false)
    case VALUE_NULL => JsonNull
    case VALUE_NUMBER_FLOAT => JsonFloat(jsonParser.getDoubleValue())
    case VALUE_NUMBER_INT => JsonInt(jsonParser.getIntValue())
    case VALUE_STRING => JsonString(jsonParser.getText())
    case VALUE_TRUE => JsonBool(true)
  }

  def readObject(jsonParser : JsonParser) : JsonObject = jsonParser.nextToken() match {
    case END_ARRAY => throw new JsonReadingException("Out of place read")
    case END_OBJECT => JsonObject(Map())
    case FIELD_NAME => {
      val key = jsonParser.getText()
      val value = read(jsonParser)
      (key -> value) :: readObject(jsonParser)
    }
    case NOT_AVAILABLE => throw new JsonReadingException("Not available")
    case START_ARRAY => throw new JsonReadingException("Out of place read")
    case START_OBJECT => throw new JsonReadingException("Out of place read")
    case VALUE_EMBEDDED_OBJECT => throw new JsonReadingException("Embedded object")
    case VALUE_FALSE => throw new JsonReadingException("Out of place read")
    case VALUE_NULL => throw new JsonReadingException("Out of place read")
    case VALUE_NUMBER_FLOAT => throw new JsonReadingException("Out of place read")
    case VALUE_NUMBER_INT => throw new JsonReadingException("Out of place read")
    case VALUE_STRING => throw new JsonReadingException("Out of place read")
    case VALUE_TRUE => throw new JsonReadingException("Out of place read")
  }

  def readArray(jsonParser : JsonParser) : JsonArray = jsonParser.nextToken() match {
    case END_ARRAY => JsonArray(Nil)
    case END_OBJECT => throw new JsonReadingException("Out of place read")
    case FIELD_NAME => throw new JsonReadingException("Out of place read")
    case NOT_AVAILABLE => throw new JsonReadingException("Not available")
    case START_ARRAY => throw new JsonReadingException("Out of place read")
    case START_OBJECT => throw new JsonReadingException("Out of place read")
    case VALUE_EMBEDDED_OBJECT => throw new JsonReadingException("Embedded object")
    case VALUE_FALSE => JsonBool(false) :: readArray(jsonParser)
    case VALUE_NULL => JsonNull :: readArray(jsonParser)
    case VALUE_NUMBER_FLOAT => JsonFloat(jsonParser.getDoubleValue()) :: readArray(jsonParser)
    case VALUE_NUMBER_INT => JsonInt(jsonParser.getIntValue()) :: readArray(jsonParser)
    case VALUE_STRING => JsonString(jsonParser.getText()) :: readArray(jsonParser)
    case VALUE_TRUE => JsonBool(true) :: readArray(jsonParser)
  }
}

case class JsonReadingException(val msg : String = "", val cause : Throwable = null) extends RuntimeException(msg, cause)
