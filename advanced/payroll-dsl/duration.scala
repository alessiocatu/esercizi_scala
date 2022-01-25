package payroll.dsl

case class Duration(val amount:Int) {
  def weeks = amount * 5

  def years = amount * 260
}
