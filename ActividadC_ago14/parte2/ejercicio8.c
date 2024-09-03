#include <stdio.h>

int main() {
    int edad;
    printf("Ingrese su edad: ");
    scanf("%d", &edad);

    if (edad < 4) {
        printf("La entrada es gratuita.\n");
    } else if (edad >= 4 && edad <= 18) {
        printf("El precio de la entrada es $5.\n");
    } else {
        printf("El precio de la entrada es $10.\n");
    }

    return 0;
}