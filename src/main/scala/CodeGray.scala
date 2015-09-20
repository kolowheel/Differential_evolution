/**
 * Created by yaroslav on 20.09.2015.
 */
object CodeGray {
  /*
  * First half of first number with second half of second number
  */
  def codeGrayMerge(first: Double, second: Double) = {
    import java.lang.Double
    val sign = if (first >= 0) 1d else -1d
    val (ffHalf, fsHalf) = extractHalves(first)
    val (sfHalf, ssHalf) = extractHalves(second)
    Double.longBitsToDouble(decode(ffHalf | (ssHalf >> 32))) * sign
  }

  def decode(n: Long) = {
    var g = 0L
    var bits = n
    while (bits > 0) {
      g ^= bits
      bits >>= 1
    }
    g
  }

  def extractHalves(n: Double): (Long, Long) = {
    import java.lang.Double
    val bits = Double.doubleToLongBits(n.abs)
    val gray = bits ^ (bits >>> 1) //convert to gray
    ((gray >>> 32) << 32, gray << 32)
  }

}
