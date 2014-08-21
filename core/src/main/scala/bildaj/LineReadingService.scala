package bildaj

import java.io._

abstract class LineReadingService(in : Reader) extends Reader {
  def start() : String
  def processLine(line : String) : String
  def end() : String

  private var buffer_size = 0
  private var buffer_offset = 0
  private var buffer = new Array[Char](4096)
  private var write_buffer_size = 0
  private var write_buffer_offset = 0
  private var write_buffer = new Array[Char](4096)
  private var status = 0

  private def printStatus() = println("[%d] \"%s\" => \"%s\"" format (status, new String(buffer, buffer_offset, buffer_size - buffer_offset),
    new String(write_buffer, write_buffer_offset, write_buffer_size - write_buffer_offset)))

  override def read(buf : Array[Char], off : Int, len : Int) : Int = {
    // Buffer closed
    if(status == -1) {
      throw new IOException("Buffer already closed")
    }

    // Header ready to load
    if(status == 0) {
      assert(buffer_size == 0 && buffer_offset == 0)

      // Get the actual string and put it in buffer
      val startString = start().toCharArray()
      if(startString.size > buffer.size) {
        buffer = new Array[Char](startString.size)
      }
      System.arraycopy(startString, 0, buffer, buffer_offset, startString.length)
      buffer_size = startString.length
      buffer_offset = 0

      if(buffer_size > 0) {
        // Write the buffer
        status = 1
      } else {
        // Go straight into the body read
        status = 2
      }
    }

    // Writing header
    if(status == 1) {
      assert(buffer_size > 0 && buffer_offset < buffer_size)

      // Write to the buf
      val len_to_write = math.min(len, buffer_size - buffer_offset)
      System.arraycopy(buffer, buffer_offset, buf, off, len_to_write)
      buffer_offset += len_to_write

      // If this finishes the header
      if(buffer_offset == buffer_size) {
        // Go to body read
        status = 2
        // Reset header buffer
        buffer_offset = 0
        buffer_size = 0
      }
      return len_to_write
    }
    
    // Ready to read
    while(status == 2) {
      assert(write_buffer_size == 0)

      // Try to read at least 2K
      if(buffer.size - buffer_offset < 1024) {
        val new_buf = new Array[Char](buffer.size + 4096)
        System.arraycopy(buffer, buffer_offset, new_buf, 0, buffer_size - buffer_offset)
        buffer = new_buf
        buffer_size -= buffer_offset
        buffer_offset = 0
      }

      // Read from previous step
      val read = in.read(buffer, buffer_offset, buffer.size - buffer_offset)
      // If that stream is terminated enter footer phase
      if(read < 0) {
        status = 4
        buffer_size = 0
        buffer_offset = 0
      } else {
        // Increase the buffer size
        buffer_size += read
      
        // i is where the cursor is
        var i = buffer_offset
        // mark is after the last '\n' or at BOL
        while(i < buffer_size) {
          if(buffer(i) == '\n') {
            // Call our subprocess
            val line = processLine(new String(buffer, buffer_offset, i - buffer_offset))
            // Write the result into write_buffer
            val chars = line.toCharArray()
            // If we need to extends write_buffer
            if(chars.length + write_buffer_size > write_buffer.size) {
              val new_buf = new Array[Char](chars.length + write_buffer_size + 4096)
              System.arraycopy(write_buffer, 0, new_buf, 0, write_buffer_size)
              write_buffer = new_buf
            }
            // Add the new data
            System.arraycopy(chars, 0, write_buffer, write_buffer_size, chars.length)
            write_buffer_size += chars.length

            // We have written some data to write_buffer, we can return
            if(chars.length > 0) {
              status = 3
            }
            // Set mark to one character after i
            buffer_offset = i + 1
          }
          i += 1
        }
      }
      // Reset buffer as needed
      if(buffer_offset != 0) {
        if(buffer_offset == buffer_size) {
          buffer_offset = 0
          buffer_size = 0
        } else {
          System.arraycopy(buffer, buffer_offset, buffer, 0, buffer_size - buffer_offset)
          buffer_size -= buffer_offset
          buffer_offset = 0
        }
      }
    }

    // Ready to write
    if(status == 3) {
      assert(write_buffer_size > write_buffer_offset)

      // How much can we write
      val len_to_write = math.min(write_buffer_size - write_buffer_offset, len)

      // Write to buf
      System.arraycopy(write_buffer, write_buffer_offset, buf, off, len_to_write)

      // Move along the buffer
      write_buffer_offset += len_to_write

      if(write_buffer_size == write_buffer_offset) {
        // We have written everything we have buffered, need to write
        write_buffer_size = 0
        write_buffer_offset = 0
        status = 2
      }
      
      return len_to_write
    }

    // Ready to read footer
    if(status == 4) {
      assert(buffer_size == 0 && buffer_offset == buffer_size)

      val endString = end().toCharArray()
      if(endString.size > buffer.size) {
        buffer = new Array[Char](endString.size)
      }
      System.arraycopy(endString, 0, buffer, buffer_offset, endString.length)
      buffer_size = endString.size
      buffer_offset = 0

      if(buffer_size > 0) {
        status = 5
      } else {
        status = 6
      }
    }

    // Writing footer
    if(status == 5) {
      assert(buffer_size > 0 && buffer_offset < buffer_size)

      val len_to_write = math.min(len, buffer_size - buffer_offset)

      System.arraycopy(buffer, buffer_offset, buf, off, len_to_write)
      buffer_offset += len_to_write
      if(buffer_offset == buffer_size) {
        status = 6
        buffer_offset = 0
        buffer_size = 0
      }
      return len_to_write
    }

    // Done
    if(status == 6) {
      return -1
    }
    throw new RuntimeException("Invalid status %d" format status)
  }

  override def close() { 
    in.close()
    status = -1 
  }
}
