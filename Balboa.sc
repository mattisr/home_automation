import $ivy.`com.fazecast:jSerialComm:2.10.3`
import $file.Crc

object Balboa {
  import com.fazecast.jSerialComm._

  val port = SerialPort.getCommPorts.drop(1).head

  port.setBaudRate(115200)
  port.setComPortTimeouts(SerialPort.TIMEOUT_READ_BLOCKING, 0, 0);

  def getBytes(port: SerialPort) = {
    val ba = new Array[Byte](port.bytesAvailable)
    port.readBytes(ba, ba.length)
    ba
  }

  def getMessages(count: Int) = {
    port.openPort()
    port.flushIOBuffers()
    val retVal = for (i <- 1 to count) yield getNextMessage(port)
    port.closePort()
    retVal
  }

  def decodeMessages(count: Int) = {
    port.openPort()
    port.flushIOBuffers()
    for (i <- 1 to count) handleMessage(getNextMessage(port))
    port.closePort()
  }

  def handleClearToSend(sm: Array[Byte]): Unit =
    println("CTS " + sm(0))

  def handleMessage(msg: Array[Byte]) = {
    if (crcOk(msg)) {
      msg match {
        case sm if sm(0) == -1 && sm(2) == 19 =>
          handleStatus(sm.drop(3).take(sm.size - 4))
        case sm if sm(2) == 6 => handleClearToSend(sm)
        case sm if sm(2) == 7 => println("NTS " + sm(0))

        case m => println(m.toList)
      }
    }

  }

  def crcOk(msg: Array[Byte]): Boolean = {
    true
  }

  def handleStatus(status: Array[Byte]) = {
    println("Status Temp: " + status(2) / 2.0)
  }

  def getNextMessage(port: SerialPort) = {
    val twoBytes = new Array[Byte](2)
    var loops = 0
    var fixed = false
    while (twoBytes(0) != 126) {
      loops += 1
      port.readBytes(twoBytes, 2)
      // Fix reading of 126,126
      if (twoBytes(0) == 126 && twoBytes(1) == 126) {
        val size = Array[Byte](1)
        port.readBytes(size, 1)
        twoBytes(1) = size(0)

        fixed = true
      }
    }
    val msg = new Array[Byte](twoBytes(1))
    port.readBytes(msg, twoBytes(1))
    twoBytes.drop(1) ++ msg.take(twoBytes(1) - 1)
  }

}

class Client {}
