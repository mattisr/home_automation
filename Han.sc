import $ivy.`com.fazecast:jSerialComm:2.10.3`
import $file.Crc

object Han {
  import com.fazecast.jSerialComm._
  // FT232R USB UART
  val mPort = SerialPort.getCommPort("/dev/ttyUSB0")
  if (mPort.getDescriptivePortName != "FT232R USB UART")
    println("Is this port ok: " + mPort.getDescriptivePortName)
    
  val mBuffer = new java.lang.StringBuffer("")

  mPort.setBaudRate(115200)
  mPort.setComPortTimeouts(SerialPort.TIMEOUT_READ_BLOCKING, 0, 0);

  def getBytes(port: SerialPort) = {
    while (port.bytesAvailable == 0) {
      Thread.sleep(2000)
    }
    val ba = new Array[Byte](port.bytesAvailable)
    port.readBytes(ba, ba.length)
    ba
  }

  def getMoreData(): String = {
    val bytes = getBytes(mPort)
    new String(bytes, java.nio.charset.StandardCharsets.ISO_8859_1)
  }

  def getMessages(no: Int) = {
    mPort.openPort()
    val msgs = for (i <- (1 to no)) yield {
      getNextMessage(getMoreData _, mBuffer)
    }
    mPort.closePort()
    msgs
  }
  
  def getNextMessage(textGenerator: () => String,
                     buffer: StringBuffer): String = {
    var msg = ""

    while (msg.isEmpty) {
      val start = buffer.indexOf("/")
      val stop = buffer.indexOf("!")
      if (start != -1 && stop != -1) {
        if (stop < start) {
          // Out of synch
	  if(buffer.length > stop + 6) 
            buffer.delete(0, stop+6)
	  else
	    buffer.append(textGenerator())  
        } else {
          // In synch, check for complete CRC line
	  if (buffer.length > stop + 6) {
	    // Complete message available
	    msg = buffer.substring(start, stop + 7)
	    buffer.delete(start, stop + 7)
	  } else {
	    // Get CRC
	    buffer.append(textGenerator())
          }
        }
      } else {
        // Need more data
        buffer.append(textGenerator())
      }
    }
    msg
  }    
}

class Client {}
