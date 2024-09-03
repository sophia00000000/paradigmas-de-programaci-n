#include <stdio.h>

int main(void) {

  int n;
  int m; 
  
  printf("Escriba n numero: ");
  scanf("%d", &n);

  printf("Escriba m numero diferente del primero: ");
  scanf("%d", &m);

  int c=n/m;
  int r=n%m;
  
  printf("El cociente es: %d\n", c);
  printf("El resto es: %d\n", r);
  
  return 0;
}
