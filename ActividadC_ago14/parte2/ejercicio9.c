#include <stdio.h>
#include <string.h>

int main() {
    char tipoPizza[20];
    char ingrediente[20];

    printf("¿Desea una pizza vegetariana? (si/no): ");
    scanf("%s", tipoPizza);

    if (strcmp(tipoPizza, "si") == 0) {
        printf("Ingredientes vegetarianos:\n");
        printf("1. Pimiento\n");
        printf("2. Tofu\n");
        printf("Elija un ingrediente (1 o 2): ");
        scanf("%s", ingrediente);

        if (strcmp(ingrediente, "1") == 0) {
            printf("Su pizza  lleva: Mozzarella, tomate y Pimiento.\n");
        } else if (strcmp(ingrediente, "2") == 0) {
            printf("Su pizza  lleva: Mozzarella, tomate y Tofu.\n");
        } else {
            printf("Opción inválida.\n");
        }
    } else if (strcmp(tipoPizza, "no") == 0) {
        printf("Ingredientes no vegetarianos:\n");
        printf("1. Peperoni\n");
        printf("2. Jamón\n");
        printf("3. Salmón\n");
        printf("Elija un ingrediente (1, 2 o 3): ");
        scanf("%s", ingrediente);

        if (strcmp(ingrediente, "1") == 0) {
            printf("Su pizza lleva: Mozzarella, tomate y Peperoni.\n");
        } else if (strcmp(ingrediente, "2") == 0) {
            printf("Su pizza lleva: Mozzarella, tomate y Jamón.\n");
        } else if (strcmp(ingrediente, "3") == 0) {
            printf("Su pizza lleva: Mozzarella, tomate y Salmón.\n");
        } else {
            printf("Opción inválida.\n");
        }
    } else {
        printf("Opción inválida.\n");
    }

    return 0;
}