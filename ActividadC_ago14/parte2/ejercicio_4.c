#include <stdio.h>

int main(void) {
  int edad;
  printf("introducir numero entero positivo: \n");
  scanf("%d", &edad);
  if(edad%2!=0)printf("El numero es impar");
  if(edad%2==0)printf("El numero es par");
  return 0;
}