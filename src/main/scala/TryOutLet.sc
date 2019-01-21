import LET.Let._
import LET._
import LET.Program

/**
  * BE SURE TO BUILD THE PROJECT BEFORE RUNNING THESE WORKSHEETS. IT DOES NOT BUILD THE PROJECT FOR YOU!
  */
object TryOutLet {
    println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet

    val p1 = Program(ConstExp(3)) //> p1  : Program = 3
    val p2 = Program(DiffExp(ConstExp(3), ConstExp(4)))
    //> p2  : Program = ( 3 - 4 )
    val p3 = Program(
        DiffExp(
            ConstExp(10),
            DiffExp(ConstExp(3), ConstExp(4)))) //> p3  : Program = ( 10 - ( 3 - 4 ) )
    // x
    val p4 = Program(VarExp('x)) //> p4  : Program = x
    // let x = 0 in x - 4
    val p5 = Program(LetExp('x, ConstExp(0), DiffExp(VarExp('x), ConstExp(4))))
    //> p5  : Program = let x = 0 in ( x - 4 )
    // let x = 0 in let x = 2 in x
    val p6 = Program(LetExp('x, ConstExp(0), LetExp('x, ConstExp(2), VarExp('x))))
    //> p6  : Program = let x = 0 in let x = 2 in x
    // xx
    val p7 = Program(VarExp('xx)) //> p7  : Program = xx
    val p8 = Program(LetExp('y, ConstExp(0), LetExp('y, DiffExp(VarExp('y), ConstExp(1)), VarExp('y))))
    //> p8  : Program = let y = 0 in let y = ( y - 1 ) in y
    val p9 = Program(ZeroExp(ConstExp(10))) //> p9  : Program = zero? 10

    val l = List(p1, p2, p3, p4, p5, p6, p8, p9) //> l  : List[Program] = List(3, ( 3 - 4 ), ( 10 - ( 3 - 4 ) ), x, let x = 0 in
    //| ( x - 4 ), let x = 0 in let x = 2 in x, let y = 0 in let y = ( y - 1 ) in y,
    //|  zero? 10)

    l.map(p => (p, Let.valueOf(p))) //> res0: List[(Program, Let.ExpVal)] = List((3,3), (( 3 - 4 ),-1), (( 10 - ( 3
    //| - 4 ) ),11), (x,10), (let x = 0 in ( x - 4 ),-4), (let x = 0 in let x = 2 in
    //|  x,2), (let y = 0 in let y = ( y - 1 ) in y,-1), (zero? 10,false))

    for (p <- l) yield {
        println(p)
        println(valueOf(p))
        valueOf(p)
    } //> 3
    //| 3
    //| ( 3 - 4 )
    //| -1
    //| ( 10 - ( 3 - 4 ) )
    //| 11
    //| x
    //| 10
    //| let x = 0 in ( x - 4 )
    //| -4
    //| let x = 0 in let x = 2 in x
    //| 2
    //| let y = 0 in let y = ( y - 1 ) in y
    //| -1
    //| zero? 10
    //| false
    //| res1: List[Let.ExpVal] = List(3, -1, 11, 10, -4, 2, -1, false)


}