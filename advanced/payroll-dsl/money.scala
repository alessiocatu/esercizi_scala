package payroll
import java.math.{BigDecimal, MathContext, RoundingMode}

class Money(val amount:BigDecimal){

}

object Money {
  def apply(amount: BigDecimal)  = new Money(amount)
  def apply(amount: Double)      = new Money(scaled(new BigDecimal(amount)))
  def apply(amount: Long)        = new Money(scaled(new BigDecimal(amount)))
  def apply(amount: Int)         = new Money(scaled(new BigDecimal(amount)))

  def unapply(m: Money) = Some(m.amount)

  protected def scaled(d: BigDecimal) = d.setScale(scale, roundingMode)

  val scale = 4
  val roundingMode  = RoundingMode.HALF_UP
  val context = new MathContext(scale, roundingMode)
}
