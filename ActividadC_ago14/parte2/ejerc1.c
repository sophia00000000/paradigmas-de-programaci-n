#include <stdio.h>

int main() {

  int edad;

  // Ask the user to type a number
  printf("Type edad: \n");

  // Get and save the number the user types
  scanf("%d", &edad);

  if (edad >= 18) {
    printf("Es mayor de edad");
  } else
    printf("no es mayor de edad");

}