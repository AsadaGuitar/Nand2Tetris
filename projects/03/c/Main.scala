
/**
 *  フローを理解したいだけなので文法は適当
 */
object Main extends App {

    def mux(a: Boolan, b: Boolean, sel: Boolean) = if (!sel) a else b

    // クロック後に返却
    val dff = (in: Boolean) => in

    def bit(in: Boolean, load: Boolean, feedback: boolean) = {
        // loadフラグがtrueの場合、inを採用
        val m = mux(feedback, in, load) 
        // 上記の処理結果をクロック時に実行
        val d = dff(m)
        // 再帰
        bit(_, load, d);
    }

    def register(in: Boolean[16], load: Boolean) =
        in map { i => bit(i, load, _) }
    

    def ram8(in: Boolean[16], load: Boolean, address: Boolean[3]) {
        1,0,1,0,1,0,0,0,
        1,0,1,0,1,1,0,0
    }

    
    val in = List(
        1,0,1,0,1,0,0,0,
        1,0,1,0,1,0,0,0
    )
    val load = true
    val address = List(0,0,0,1,0,0)

    val x = List(0,1,0)
    val y = List(1,0,0)

    val (a,b,c,d,e,f,g,h) = dmux(in,y)
    /**
     *  a   0
     *  b   0
     *  c   0   
     *  d   0
     *  e   1
     *  f   0
     *  g   0
     *  h   0
     */

     val ra = ram8(in, a, x)
     val ra = ram8(in, b, x)
     val ra = ram8(in, c, x)
     val ra = ram8(in, d, x)
     val ra = ram8(in, e, x) 
     //  1,0,1,0,1,0,0,0,
     //  1,0,1,0,1,1,0,0
     val ra = ram8(in, f, x)
     val ra = ram8(in, g, x)
     val ra = ram8(in, h, x)
     
     val r = mux8way16(ra,rb,rc...., sel=y)
}