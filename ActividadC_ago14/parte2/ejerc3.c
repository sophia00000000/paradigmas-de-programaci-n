#include <stdio.h>

int main() {
    float num1; 
    float num2;

    printf("Ingrese el primer dígito (dividendo): ");
    scanf("%f", &num1);
    printf("Ingrese el segundo dígito (divisor): ");
    scanf("%f", &num2);

    if (num2 == 0) {
        printf("Error: División por cero.");
    } else {
        printf("%.2f", num1 / num2);
    }

    return 0;
}