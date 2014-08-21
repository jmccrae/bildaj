package bildaj

import org.scalatest._
import com.fasterxml.jackson.core._

class JsonTest extends FlatSpec with Matchers {
  "json" should "read object" in {
    val jsonString = """
    { 
      "w": {
        "a" : "b"
      },
      "x": true,
      "y": null,
      "z": [1,"2",3.0]
    }"""
    val jsonParser = new JsonFactory().createParser(new java.io.StringReader(jsonString))
    
    val result = JsonUtils.read(jsonParser)

    result should be (JsonObject(Map("w" -> JsonObject(Map("a" -> JsonString("b"))), "x" -> JsonBool(true),
      "y" -> JsonNull, "z" -> JsonArray(List(JsonInt(1), JsonString("2"), JsonFloat(3.0))))))
  }
}
