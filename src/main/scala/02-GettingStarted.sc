//var l1 = List ("Ali", "Abbas")
//"ahmad" :: l1
//l1 :+= "Ahmad"
//import scala.sys.process._
//Runtime.getRuntime().exec("notepad.exe")

// Ex2.2: Implement a polymorphic function to check whether
// an `Array[A]` is sorted
def isSorted[A] (as: Array[A] , ordered: (A,A) => Boolean) : Boolean = {
  @annotation.tailrec
  def loop (n: Int): Boolean ={
    if (n >= as.length) true
    else {if(ordered(as(n),as(n-1))) loop(n+1)
    else false}
  }
  loop(1)
}

def isSorted2[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean =
    if (n >= as.length-1) true
    else if (gt(as(n), as(n+1))) false
    else go(n+1)

  go(0)
}

isSorted(Array() , (a:Int,b:Int) => a>b )
isSorted2(Array() , (a:Int,b:Int) => a>b )
isSorted(Array(2) , (a:Int,b:Int) => a>b )
isSorted2(Array(2) , (a:Int,b:Int) => a>b )
isSorted(Array(2,3) , (a:Int,b:Int) => a>b )
isSorted2(Array(2,3) , (a:Int,b:Int) => a>b )
isSorted(Array(3,2) , (a:Int,b:Int) => a>b )
isSorted2(Array(3,2) , (a:Int,b:Int) => a>b )
isSorted(Array(2,3,4) , (a:Int,b:Int) => a>b )
isSorted2(Array(2,3,4) , (a:Int,b:Int) => a>b )
isSorted(Array(2,3,4) , (a:Int,b:Int) => a<b )
isSorted2(Array(2,3,4) , (a:Int,b:Int) => a<b )
isSorted(Array(4,3,2) , (a:Int,b:Int) => a<b )
isSorted2(Array(4,3,2) , (a:Int,b:Int) => a<b )
isSorted(Array('a','b','c') , (a:Char,b:Char) => a>b )
isSorted2(Array('a','b','c') , (a:Char,b:Char) => a>b )
isSorted(Array('a','c','b') , (a:Char,b:Char) => a>b )
isSorted2(Array('a','c','b') , (a:Char,b:Char) => a>b )
isSorted(Array("abc","def","ghi") , (a:String,b:String) => a>b )
isSorted2(Array("abc","def","ghi") , (a:String,b:String) => a>b )

println("_______________________")

// Ex2.3: Implement `curry`.
def curry[A,B,C](f: (A, B) => C): A => (B => C) =
a => b => f(a, b)

// Ex2.4: Implement `uncurry`
def uncurry[A,B,C](f: A => B => C): (A, B) => C =
(a, b) => f(a)(b)

// Ex2.5: Implement `compose`
def compose[A,B,C](f: B => C, g: A => B): A => C =
a => f(g(a))

