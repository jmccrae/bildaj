package bildaj

import java.io.{File, FileReader, FileWriter, StringWriter}

class FileSource(f : File) extends StreamSource with StreamSink {
  def in = new FileWriter(f)
  def out(implicit context : ServiceContext) = new FileReader(f)
}

object file {
  def apply(f : File) = new FileSource(f)
  def apply(f : String) = new FileSource(new File(f))
}

class StringBufferSink() extends StreamSink {
  lazy val in = new StringWriter()
  override def toString = in.toString
}
