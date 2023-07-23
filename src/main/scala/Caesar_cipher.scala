import scala.io.StdIn

object Caesar_cipher {
  def main(args: Array[String]): Unit = {
    val plaintext = StdIn.readLine("Enter your plain text : ").toString
    val shift = StdIn.readLine("Enter shift value (A positive number between 0 and 26) : ").toInt

    println("Plain Text = " + plaintext)

    val encryptedText = Encryption(plaintext, shift)
    println("Encrypted Text = " + encryptedText)

    val decryptedText = Decryption(encryptedText, shift)
    println("Decrypted Text = " + decryptedText)
  }


  def Encryption(plaintext: String, shift: Int): String = {
    val encryptedText = plaintext.map { char =>
      if (char.isUpper) {
        val shiftedChar = ((char - 'A' + shift) % 26 + 'A').toChar
        shiftedChar
      }
      else if (char.isLower) {
        val shiftedChar = ((char - 'a' + shift) % 26 + 'a').toChar
        shiftedChar
      } else {
        char
      }
    }
    encryptedText
  }

  def Decryption(encryptedText: String, shift: Int): String = {
    val decryptedText = encryptedText.map { char =>
      if (char.isUpper) {
        val shiftedChar = ((char - 'A' - shift + 26) % 26 + 'A').toChar
        shiftedChar }
       else if (char.isLower) {
          val shiftedChar = ((char - 'a' - shift + 26) % 26 + 'a').toChar
          shiftedChar
      } else {
        char
      }
    }
    decryptedText

  }

}
