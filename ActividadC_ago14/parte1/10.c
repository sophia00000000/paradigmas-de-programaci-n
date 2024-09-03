#include <stdio.h>

int main() {
    int numPayasos, numMuñecas;
    int pesoPayasos, pesoMuñecas, pesoTotal;

    printf("Ingrese el número de payasos: ");
    scanf("%d", &numPayasos);

    printf("Ingrese el número de muñecas: ");
    scanf("%d", &numMuñecas);

    pesoPayasos = numPayasos * 112;
    pesoMuñecas = numMuñecas * 75;

    pesoTotal = pesoPayasos + pesoMuñecas;

    printf("El peso total del paquete es: %d gramos\n", pesoTotal);

    return 0;
}