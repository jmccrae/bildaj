package bildaj

import java.io.{File, OutputStream, Writer}
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator, JsonEncoding}

class JsonGen(jsonGenerator : JsonGenerator) {
  import JsonGen._
  
  def this(f : File) = this(
    new JsonFactory().createJsonGenerator(f, JsonEncoding.UTF8)
  )

  def this(o : OutputStream) = this(
    new JsonFactory().createJsonGenerator(o)
  )

  def this(w : Writer) = this(
    new JsonFactory().createJsonGenerator(w)
  )

  def pretty : JsonGen = {
    jsonGenerator.useDefaultPrettyPrinter()
    return this
  }

  def obj(elems : (String, JsonGenElem)*) = JsonGen.obj(elems:_*).generate(jsonGenerator)

  def array(elems : (JsonGenElem)*) = JsonGen.array(elems:_*).generate(jsonGenerator)

}

object JsonGen {
  trait JsonGenElem {
    def generate(jsonGenerator : JsonGenerator) : Unit
  }

  def obj(elems : (String, JsonGenElem)*) : JsonGenElem = new JsonGenElem {
    def generate(jsonGenerator : JsonGenerator) {
      jsonGenerator.writeStartObject()
      for((k,e) <- elems) {
        jsonGenerator.writeFieldName(k)
        e.generate(jsonGenerator)
      }
      jsonGenerator.writeEndObject()
      jsonGenerator.flush()
    }
  }

  def array(elems : JsonGenElem*) : JsonGenElem = new JsonGenElem {
    def generate(jsonGenerator : JsonGenerator) {
      jsonGenerator.writeStartArray()
      for(e <- elems) {
        e.generate(jsonGenerator)
      }
      jsonGenerator.writeEndArray()    
    }
  }

  implicit class StringJsonGen(s : String) extends JsonGenElem {
    def generate(jsonGenerator : JsonGenerator) {
      jsonGenerator.writeString(s)
    }
  }
}
