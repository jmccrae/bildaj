package bildaj

import org.scalatest._

class TestTxt2Nif extends FlatSpec with Matchers {
  "txt2nif" should "convert a text document to nif" in {
    implicit val context = DefaultServiceContext

    val stringbuffer = new StringBufferSink()

    file("core/src/test/resources/example.txt") | txt2nif | stringbuffer

    stringbuffer.toString.contains("events,") should be (true)
  }
}

