package bildaj

import org.scalatest._

class TestJsonGen extends FlatSpec with Matchers {
  "JsonGen" should "generate valid JSON" in {
    import JsonGen._

    val sw = new java.io.StringWriter()

    new JsonGen(sw).obj {
        "foo" -> array("bar\ntest","xxx")
      }
    sw.toString() should be ("{\"foo\":[\"bar\\ntest\",\"xxx\"]}")
  }
}
