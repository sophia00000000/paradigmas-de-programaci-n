#include <stdio.h>

int main() {
  double montoDinero;
  printf("Introduzca el monto depositado: ");
  scanf("%lf", &montoDinero);

  double ano1 = montoDinero * 1.04;
  double ano2 = ano1 * 1.04;
  double ano3 = ano2 * 1.04;

  printf("Desùes del primer año: %.2lf\n", ano1);
  printf("Desùes del segundo año: %.2lf\n", ano2);
  printf("Desùes del tercer año: %.2lf\n", ano3);

  return 0;
}