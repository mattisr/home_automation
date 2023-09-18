	val poly : Int = 0x007;
	var crc: Int = 0;

	@Override
	def update(input: Array[Byte] , offset: Int, len: Int): Unit = {
		for (i <- 0 until len) {
			update(input(offset + i));
		}
	}

	def update(input: Array[Byte]): Unit = {
		update(input, 0, input.length);
	}

	def update(b: Byte): Unit = {
		crc = crc ^ b;
		for (j <- 0 to 7) {
			if ((crc & 0x80) != 0) {
				crc = ((crc << 1) ^ poly);
			} else {
				crc <<= 1;
			}
		}
		crc &= 0xFF;
	}

	def getValue(): Int = {
		return (crc & 0xFF);
	}

	def reset(init: Int = 0): Unit = {
		crc = init;
	}
