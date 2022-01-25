package payroll.dsl.rules

def rules = Rules

class Rules extends Decapsulation (f: (employee:Emplyee) => Unit){}
object Rules extends Decapsulation {}
