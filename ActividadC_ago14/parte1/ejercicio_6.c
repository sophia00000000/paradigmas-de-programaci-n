#include <stdio.h>


int main(void) {
  int num;
  int result;
  printf("introducir numero entero positivo: \n");
  scanf("%d", &num);
  result=num*(num+1)/2;
   printf("%d", result);
  return 0;
}