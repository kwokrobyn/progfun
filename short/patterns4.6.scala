
trait Expr 
case class Number(n: Int) extends Expr 
case class Sum(e1: Expr, e2: Expr) extends Expr 
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(s: String) extends Expr

object exprs {

    def show(e: Expr):String = e match {
        case Number(n) => n.toString
        case Sum(x, y) => show(x) + " + " + show(y)
        case Prod(x, y) => (x match {
            case Sum(i, j) => "(" + show(x) + ")"
            case _ => show(x)
        }) + "*" + (y match {
            case Sum(i, j) => "(" + show(y) + ")"
            case _ => show(y)
        })
        case Var(s) => s
    }

    def eval(e: Expr): Int = e match {
        case Number(n) => n 
        case Sum(x, y) => eval(x) + eval(y)
        case Prod(x, y) => eval(x) * eval(y)
    }

    val a = Sum(Prod(Number(2), Var("x")), Var("y"))
    val b = Prod(Sum(Number(2), Var("x")), Var("y"))

    show(a)
    show(b)

}