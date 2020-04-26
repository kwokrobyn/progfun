abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat 
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat 
    def - (that: Nat): Nat 
}

object Zero extends Nat {
    def isZero: Boolean = true 
    def predecessor: Nat = throw new NoSuchElementException
    def + (that: Nat) = that 
    def - (that: Nat) = if (that.isZero) that else throw new NoSuchElementException
}

class Succ(n: Nat) extends Nat {
    def isZero: Boolean = false 
    def predecessor: Nat = n 
    def + (that: Nat): Nat = new Succ(n + that)
    def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
    
}

// one + two 
// = new Succ(zero + two)
// = new Succ(two)


val zero = Zero 
val one = new Succ(zero)
val two = new Succ(one)
val three = new Succ(two)
val x = one + two
val y = three - x
