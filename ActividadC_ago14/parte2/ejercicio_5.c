#include <stdio.h>

int main(void) {
  int edad;
  int ingresos;
  printf("introducir edad en años: \n");
  scanf("%d", &edad);
   printf("introducir ingresos mensuales: \n");
  scanf("%d", &ingresos);
  if (edad<16 || ingresos<1000)   printf(" NO DEBE TRIBUTAR \n");
  if (edad >=16 && ingresos>=1000) printf(" SI DEBE TRIBUTAR \n");
  return 0;
}
