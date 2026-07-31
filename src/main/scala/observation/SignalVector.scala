package HT.observation

class SignalVector(val vector: String, val size: Int) {
    val lowerCaseVector: String = vector.toLowerCase
    val extendedVector: String = {
        if (lowerCaseVector.length < size) {
            val paddingChar = if (lowerCaseVector.nonEmpty && (lowerCaseVector.head == 'x' || lowerCaseVector.head == 'z')) lowerCaseVector.head else '0'
            lowerCaseVector.reverse.padTo(size, paddingChar).reverse
        } else {
            lowerCaseVector
        }
    }

    require(lowerCaseVector.forall(c => c == '0' || c == '1' || c == 'x' || c == 'z'), 
        "Vector can only contain characters 0, 1, x, or z")
    require(extendedVector.length() == size, 
        s"Vector length ${extendedVector.length} does not match expected size $size")

    def charAt(index: Int): Char = extendedVector.charAt(size - index - 1)

    def length: Int = size

    override def toString(): String = {extendedVector}

    def toBigInt(signed: Boolean = false): BigInt = {
        if (!isWellDefined) {
            println("GlobalCycle " + GlobalCycle)
            println(extendedVector)
            return 0
            // throw new IllegalArgumentException("Vector contains invalid characters 'x' or 'z' for conversion to BigInt")
        }
        val bigIntValue = BigInt(extendedVector, 2)
        if (signed && extendedVector.head == '1') {
            bigIntValue - (BigInt(1) << size)
        } else {
            bigIntValue
        }
    }

    def isWellDefined: Boolean = {
        !extendedVector.exists(c => c == 'x' || c == 'z')
    }
    
    def asBoolean: Boolean = {
        assert(size == 1)
        vector == "1"
    }
}
