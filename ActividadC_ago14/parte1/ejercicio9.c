#include <stdio.h>

int main(void) {

  int a,b,c;

printf("Indoduzca la cantidad que desea invertir: ");
scanf("%d", &a);

printf("Introduzca el interes anual: ");
scanf("%d",&b);

printf("Introduzca el numero de años: ");
scanf("%d",&c);

printf("El capital obtenido es: %d ", a*b*c);
                 
  return 0;
}
