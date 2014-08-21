package bildaj

import java.io._

class Txt2Nif(in : Reader) extends LineReadingService(in) {
  private var endIndex = 0

  def start() = """{
  "@context": "http://liderproject.github.io/bildaj/contexts/nif.json","
  "rdf:type": ["nif:RFC5147String", "nif:Context"],
  "beginIndex": 0,
  "isString \": """ + "\""
  
  def processLine(line : String) = {
    endIndex += line.size + 1
    line.replaceAll("\"","\\\"") + "\\n"
  }

  def end() = """\",
  "endIndex": %d
}""" format endIndex
}

object txt2nif extends StreamService {
  protected def makeService(out : Reader) = new Txt2Nif(out) 
}
