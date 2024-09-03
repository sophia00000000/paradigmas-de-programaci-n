public class Algoritmos {
    public static void main(String[] args) {
        System.out.println(Factorial(6));
        System.out.println(Euclides(10,12));
        System.out.println(Sucesora(8));
        System.out.println(Suma(7,3));
        System.out.println(Multiplicacion(4,7));
    }
     public static int Factorial (int n){
        int r=1;
        for (int i=n; i>0;i--){
            r*=i;
        }
        return r;
    }   
    
    public static int Euclides (int a, int b){
        int x=0;
        int y = 0;
        int mcd=0;
        int mcm=0;

        if (a>0&&b>0) {
            if(a>b){
                x=a;
                y=b;
            }
            else{
                x=b;
                y=a;
            }
            
            while(y!=0){
                mcd=y;
                y=(x%y);
                x=mcd;
            }
            
            mcm=(a*b)/mcd;
        }             
        return mcm;
    }  
    
    public static int Sucesora (int n){
        return n+1;
    }
    
    public static int Suma (int x, int y){
        int resultado=x;
        for (int i = 0; i < y; i++) {
            resultado=Sucesora(resultado);
        }
        return resultado;
    }
    
    public static int Multiplicacion (int x, int y){
        int resultado=0;
        for (int i = 0; i < y; i++) {
            resultado=Suma(resultado,x);
        }
        return resultado;
    }
}
