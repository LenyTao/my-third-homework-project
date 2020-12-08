import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait Block {
  def length: Int
}

case class UncompressedBlock(length: Int, data: Seq[Char]) extends Block

case class CompressedBlock(length: Int, data: Char) extends Block

class RleEncoding {

  def encode(str: String): Seq[Block] = {

    var collectionBlock = new ListBuffer[Block]
    val (_, optBlock, result) = {
      str.toCharArray.foldLeft((None: Option[Char], None: Option[Block], Seq.empty[Block])) {
        /** Пустой лист или лист с одним значением */

        case ((None, None, result), char) if str.length == 1 =>
          collectionBlock += UncompressedBlock(1, Seq(char)); (Some(char), Some(UncompressedBlock(1, Seq(char))), result)

        case ((None, _, result), char) =>
          (Some(char), None, result)

        /** Обработка Uncompress */

        case ((Some(prev), None, result), char) if prev != char =>
          (Some(char), Some(UncompressedBlock(2, Seq(prev, char))), result)

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev != char =>
          (Some(char), Some(UncompressedBlock(block.length + 1, block.data :+ char)), result)

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) =>
          collectionBlock += UncompressedBlock(block.length - 1, block.data.dropRight(1)); (Some(char), Some(CompressedBlock(2, prev)), result :+ block)

        /** Обработка Compress */

        case ((Some(prev), None, result), char) if prev == char =>
          (Some(char), Some(CompressedBlock(2, prev)), result)

        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev == char =>
          (Some(char), Some(CompressedBlock(block.length + 1, block.data)), result)

        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev != char =>
          collectionBlock += block; (Some(char), None, result :+ block)
      }
    }

    if (str.length > 1) {
      if (str(str.length - 2) == str(str.length - 3) && str(str.length - 1) != str(str.length - 2)) {
        collectionBlock += UncompressedBlock(1, Seq(str(str.length - 1)))
      }
      else {
        collectionBlock += optBlock.get
      }
    }
    collectionBlock.toSeq
  }
}


object RleEncodingApp extends App {

  val rleEncoding = new RleEncoding
  println(rleEncoding.encode(""))
  println(rleEncoding.encode("X"))
  println(rleEncoding.encode("ABCDE"))
  println(rleEncoding.encode("AABBCCDD"))
  println(rleEncoding.encode("XAABBCCDD"))
  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB"))
  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX"))

}