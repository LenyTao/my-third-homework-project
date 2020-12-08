import org.scalatest.flatspec._
import org.scalatest.matchers._

class RleEncodingSpec extends AnyFlatSpec with should.Matchers {

  "RleEncoding" should "encode given string1" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("ABCDE") should be (List(UncompressedBlock(5,List('A', 'B', 'C', 'D', 'E'))))
  }

  it should "encode given string2" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("AABBCCDD") should be (List(CompressedBlock(2,'A'), CompressedBlock(2,'B'), CompressedBlock(2,'C'), CompressedBlock(2,'D')))
  }

  it should "encode given string3" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("XAABBCCDD") should be (List(UncompressedBlock(1,List('X')), CompressedBlock(2,'A'), CompressedBlock(2,'B'), CompressedBlock(2,'C'), CompressedBlock(2,'D')))
  }
  it should "encode given string4" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB") should be (List(CompressedBlock(4,'A'), CompressedBlock(3,'B'), CompressedBlock(2,'C'), UncompressedBlock(3,List('X', 'Y', 'Z')), CompressedBlock(4,'D'), CompressedBlock(3,'E'), CompressedBlock(3,'F'), CompressedBlock(6,'A'), CompressedBlock(29,'B')))
  }
  it should "encode given string5" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX") should be (List(CompressedBlock(4,'A'), CompressedBlock(3,'B'), CompressedBlock(2,'C'), UncompressedBlock(3,List('X', 'Y', 'Z')), CompressedBlock(4,'D'), CompressedBlock(3,'E'), CompressedBlock(3,'F'), CompressedBlock(6,'A'), CompressedBlock(29,'B'), UncompressedBlock(1,List('X'))))
  }

  it should "encode given string6" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("X") should be (List(UncompressedBlock(1,List('X'))))
  }
  it should "encode given string7" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("") should be (List())
  }
}