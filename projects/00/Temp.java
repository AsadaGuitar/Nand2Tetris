public class Temp {
    public void main(String[] args) {

    }

    boolean mux(boolean a, boolean b, boolean sel) {
        var x = b && sel;
        var nSel = !sel;
        var y = a && nSel;

        return x || y;
    }


    AluResult alu(boolean[] x, boolean[] y, 
        boolean zx, boolean nx, boolean zy, boolean ny, 
        boolean f, boolean no) {

            boolean out, zr, ng;

            boolean temp;
            if (zx) temp = false;
            var notx = !x;
            if (nx) temp = notx;
            
    }

    private class AluResult {
        public boolean[] out;
        public boolean zr;
        public boolean ng;
        


        public AluResult(boolean[] out, boolean zr, boolean ng) {
            this.out = out;
            this.zr = zr;
            this.ng = ng;
        }
    }
}