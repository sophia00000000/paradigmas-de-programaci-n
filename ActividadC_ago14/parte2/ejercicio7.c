#include <stdio.h>

int main() {
    double puntos;
    printf("Ingrese la puntuación obtenida por el                  empleado: ");
    scanf("%lf", &puntos);

    if (puntos == 0.0) {
        printf("Rendimiento del empleado: Básico\n");
        printf("Dinero ganado: $230.000\n");
    } else if (puntos == 0.4) {
        printf("Rendimiento del empleado: Intermedio\n");
        printf("Dinero ganado: $500.000\n");
    } else if (puntos == 0.6) {
        printf("Rendimiento del empleado: Avanzado\n");
        printf("Dinero ganado: $1¨000.000\n");
    } else if (puntos >= 0.8) {
        printf("Rendimiento del empleado: Experto\n");
        printf("Dinero ganado: $2¨000.000\n");
    } else {
        printf("Entrada no valida.\n");
    }

    return 0;
}