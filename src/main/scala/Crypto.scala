import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import java.security.SecureRandom
import java.util.Base64
import scala.util.Try

object Crypto:
  private val ALGORITHM = "AES/GCM/NoPadding"
  private val TAG_LENGTH_BIT = 128
  private val IV_LENGTH_BYTE = 12
  private val random = new SecureRandom()
  
  def generateKey(): Secret =
    val key = new Array[Byte](32)
    random.nextBytes(key)
    Secret(Base64.getEncoder.encodeToString(key))
  
  def parseKey(keyStr: String): Either[AppError, SecretKeySpec] =
    Try {
      val decoded = Base64.getDecoder.decode(keyStr.trim)
      if decoded.length != 32 then throw new IllegalArgumentException("Ключ должен быть длиной 32 байта")
      SecretKeySpec(decoded, "AES")
    }.toEither.left.map(_ => AppError.WrongKey)
  
  def encrypt(data: Array[Byte], key: SecretKeySpec): Array[Byte] =
    val iv = new Array[Byte](IV_LENGTH_BYTE)
    random.nextBytes(iv)

    val cipher = Cipher.getInstance(ALGORITHM)
    cipher.init(Cipher.ENCRYPT_MODE, key, GCMParameterSpec(TAG_LENGTH_BIT, iv))

    val ciphertext = cipher.doFinal(data)
    iv ++ ciphertext
  
  def decrypt(encryptedData: Array[Byte], key: SecretKeySpec): Either[AppError, Array[Byte]] =
    Try {
      val iv = encryptedData.take(IV_LENGTH_BYTE)
      val ciphertext = encryptedData.drop(IV_LENGTH_BYTE)

      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.DECRYPT_MODE, key, GCMParameterSpec(TAG_LENGTH_BIT, iv))
      cipher.doFinal(ciphertext)
    }.toEither.left.map(_ => AppError.WrongKey)