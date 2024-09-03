#include <stdio.h>


int main(void) {
  int horas;
  int costo;
  int total;
  printf("Horas Trabajadas\n");
  scanf("%d", &horas);
  printf("Costo hora\n");
  scanf("%d", &costo);
  total = horas * costo;
  printf("Pago total:");
  printf("%d", total);
  return 0;
}