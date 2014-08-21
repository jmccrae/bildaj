package bildaj

import java.io.{Reader, Writer}

trait StreamSource {
  def |(ss : StreamService)(implicit context : ServiceContext) : StreamSource = {
    ss.createWithSource(this)
  }
  def |(ss : StreamSink)(implicit context : ServiceContext) : Unit = {
    val _out = out(context)
    val _in = ss.in
    val buf = new Array[Char](4096)
    var read = 0
    while({read = _out.read(buf); read >= 0}) {
      _in.write(buf,0,read);
    }
    _out.close
    _in.flush
    _in.close
  }


  def out(implicit context : ServiceContext) : Reader
}

trait StreamSink {
  def in : Writer
}

trait StreamService {
  def createWithSource(src : StreamSource) = new StreamSource {
    def out(implicit context : ServiceContext) : Reader = {
      makeService(src.out)
    }
  }

  protected def makeService(out : Reader) : Reader
}

trait ServiceContext {
  def start(process : Runnable) : Unit
}

object DefaultServiceContext extends ServiceContext {
  def start(process : Runnable) = {
    java.util.concurrent.Executors.newSingleThreadExecutor().submit(process)
  }
}


