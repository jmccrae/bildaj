package bildaj

import com.fasterxml.jackson.core._
import com.fasterxml.jackson.core.JsonToken._
import java.io._

trait JsonEventHandler {
  def startObject
  def endObject
  def field(name : String)

  def startArray
  def endArray

  def value(bool : Boolean)
  def value(float : Double)
  def value(int : Int)
  def value(string : String)
  def nullValue

  def embeddedObject(obj : Object)
}

trait IdentityJsonEventHandler {
  protected var requireComma : List[Boolean] = Nil

  protected def checkComma(gen : String) = if(requireComma.head) {
    "," + gen
  } else {
    requireComma = true :: requireComma.tail
    gen
  }
  protected def pushComma(gen : String) = {
    requireComma ::= false
    gen
  }
  
  protected def popComma(gen : String) = {
    requireComma = requireComma.tail
    gen
  }


  def startObject = pushComma("{")
  def endObject = popComma("}")
  def field(name : String) = checkComma("\"%s\":" format name)
  
  def startArray = pushComma("[")
  def endArray = popComma("]")

  def value(bool : Boolean) = checkComma(if(bool) { "true" } else { "false" })
  def value(float : Double) = checkComma(float.toString)
  def value(int : Int) = checkComma(int.toString)
  def value(string : String) = checkComma("\"%s\"" format string.replaceAll("\"","\\\""))
  def nullValue = checkComma("null")

  def embeddedObject(obj : Object) = checkComma(obj.toString)
}

abstract class JsonService(in : Reader) extends Reader {
  def handler : JsonEventHandler
  private val jsonParser = new JsonFactory().createJsonParser(in)
  val out = new CharArrayWriter()
  private var offset = 0
  private var status = 0

  override def read(buf : Array[Char], off : Int, len : Int) : Int = {
    if(status == -1) {
      throw new IOException("Reader closed")
    } else if(status == 0) {
      var token : JsonToken = null
      while({ token = jsonParser.nextToken; token != null}) {
        token match {
          case END_ARRAY => handler.endArray
          case END_OBJECT => handler.endObject
          case FIELD_NAME => handler.field(jsonParser.getText())
          case NOT_AVAILABLE => throw new RuntimeException("Not available")
          case START_ARRAY => handler.startArray
          case START_OBJECT => handler.startObject
          case VALUE_EMBEDDED_OBJECT => handler.embeddedObject(jsonParser.getEmbeddedObject())
          case VALUE_FALSE => handler.value(false)
          case VALUE_NULL => handler.nullValue
          case VALUE_NUMBER_FLOAT => handler.value(jsonParser.getDoubleValue())
          case VALUE_NUMBER_INT => handler.value(jsonParser.getIntValue())
          case VALUE_STRING => handler.value(jsonParser.getText())
          case VALUE_TRUE => handler.value(true)
        }
        if(out.size > 0) {
          val chars = out.toCharArray
          if(chars.size > len) {
            System.arraycopy(chars, 0, buf, off, len);
            offset = len
            status = 1

          } else {
            System.arraycopy(chars, 0, buf, off, chars.size)
            out.reset()
            return chars.size
          }
        }
      }
      status = 2
      return -1
    } else if(status == 1) {
      val chars = out.toCharArray
      if(chars.size - offset > len) {
        System.arraycopy(chars, offset, buf, off, len)
        offset += len
        return len
      } else {
        val size_to_copy = chars.size - offset
        System.arraycopy(chars, offset, buf, off, size_to_copy)
        offset = 0
        out.reset()
        status = 0
        return size_to_copy
      }
    } else if(status == 2) {
      return -1
    } else {
      throw new RuntimeException("status = %d" format status)
    }
  } 

  override def close() {
    jsonParser.close()
    status = -1
  }
}

