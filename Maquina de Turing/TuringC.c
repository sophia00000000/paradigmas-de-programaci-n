#include <stdio.h>
#include <string.h>

#define MAX_TAPE_SIZE 100
#define RESULT_TAPE_SIZE (MAX_TAPE_SIZE * 2)  // Doble tamaño para el resultado

typedef enum {
    q0,   // Estado inicial
    q1,   // Leer bits de las dos cintas
    q2,   // Manejar acarreo o préstamo
    q3,   // Escribir el resultado en la cinta
    q4,   // Mover cabezales
    q5,   // Estado final
} Estado;

typedef struct {
    int cinta1[MAX_TAPE_SIZE];
    int cinta2[MAX_TAPE_SIZE];
    int resultado[RESULT_TAPE_SIZE];
    int cabezal1;
    int cabezal2;
    int cabezalResultado;
    int tam_entrada;
    int tam_resultado;
    Estado estado;
    char operacion;
    int acarreo;
    int prestamo;
} MaquinaTuring;

void inicializarMT(MaquinaTuring *mt, int cinta1[], int cinta2[], int tam, char op) {
    memcpy(mt->cinta1, cinta1, tam * sizeof(int));
    memcpy(mt->cinta2, cinta2, tam * sizeof(int));
    memset(mt->resultado, 0, RESULT_TAPE_SIZE * sizeof(int));
    mt->cabezal1 = tam - 1;  // Empezamos desde el bit menos significativo (derecha)
    mt->cabezal2 = tam - 1;
    mt->cabezalResultado = RESULT_TAPE_SIZE - 1;
    mt->tam_entrada = tam;
    mt->tam_resultado = RESULT_TAPE_SIZE;
    mt->estado = q0;
    mt->operacion = op;
    mt->acarreo = 0;
    mt->prestamo = 0;
}

void transicion(MaquinaTuring *mt) {
    switch (mt->estado) {
        case q0:
            mt->estado = q1;  // Leer bits
            break;

        case q1:
            // Leer los bits actuales de las dos cintas o si queda acarreo
            if (mt->cabezal1 >= 0 || mt->cabezal2 >= 0 || mt->acarreo > 0) {
                mt->estado = q2;  // Pasar al manejo de acarreo o préstamo
            } else {
                mt->estado = q5;  // Terminar si ya no quedan bits ni acarreo
            }
            break;

        case q2:
            // Suma o resta según la operación
            if (mt->operacion == '+') {
                int bit1 = (mt->cabezal1 >= 0) ? mt->cinta1[mt->cabezal1] : 0;
                int bit2 = (mt->cabezal2 >= 0) ? mt->cinta2[mt->cabezal2] : 0;
                int suma = bit1 + bit2 + mt->acarreo;

                if (suma == 0 || suma == 1) {
                    mt->resultado[mt->cabezalResultado] = suma;
                    mt->acarreo = 0;
                } else if (suma == 2) {
                    mt->resultado[mt->cabezalResultado] = 0;
                    mt->acarreo = 1;
                } else if (suma == 3) {
                    mt->resultado[mt->cabezalResultado] = 1;
                    mt->acarreo = 1;
                }
            } else if (mt->operacion == '-') {
                int bit1 = (mt->cabezal1 >= 0) ? mt->cinta1[mt->cabezal1] - mt->prestamo : 0;
                int bit2 = (mt->cabezal2 >= 0) ? mt->cinta2[mt->cabezal2] : 0;

                if (bit1 >= bit2) {
                    mt->resultado[mt->cabezalResultado] = bit1 - bit2;
                    mt->prestamo = 0;
                } else {
                    mt->resultado[mt->cabezalResultado] = (bit1 + 2) - bit2;  // Prestar de la siguiente posición
                    mt->prestamo = 1;
                }
            }
            mt->estado = q3;  // Pasar a escribir el resultado
            break;

        case q3:
        mt->resultado[mt->cabezalResultado] = mt->resultado[mt->cabezalResultado];
            mt->estado = q4;  // Pasar a mover el cabezal
            break;

        case q4:
            // Mover los cabezales hacia la izquierda
            mt->cabezal1--;
            mt->cabezal2--;
            mt->cabezalResultado--;

            // Volver al estado de lectura de bits
            mt->estado = q1;
            break;

        case q5:
            // Estado final, no hacer nada
            break;
    }
}

void ejecutarMT(MaquinaTuring *mt) {
    while (mt->estado != q5) {
        transicion(mt);
    }
}

void imprimirBinario(int binario[], int tam) {
    int inicio = 0;
    // Encuentra el primer 1 para omitir ceros a la izquierda
    while (inicio < tam && binario[inicio] == 0) {
        inicio++;
    }
    if (inicio == tam) {
        printf("0");  // Si todo es cero, imprime un solo 0
    } else {
        for (int i = inicio; i < tam; i++) {
            printf("%d", binario[i]);
        }
    }
}

int main() {
    int cinta1[] = {1, 0, 0, 0};  // 120 en decimal
    int cinta2[] = {0, 1, 0, 1};  // 36 en decimal
    int tam = sizeof(cinta1) / sizeof(cinta1[0]);
    MaquinaTuring mt;

    printf("Número 1: ");
    imprimirBinario(cinta1, tam);
    printf("\nNúmero 2: ");
    imprimirBinario(cinta2, tam);
    printf("\n\n");

    char operaciones[] = {'+', '-'};
    char* nombres_operaciones[] = {"suma", "resta"};

    for (int i = 0; i < 2; i++) {
        inicializarMT(&mt, cinta1, cinta2, tam, operaciones[i]);
        ejecutarMT(&mt);

        printf("Resultado de la %s:\n", nombres_operaciones[i]);
        imprimirBinario(cinta1, tam);  // Imprimir cinta1
        printf(" %c ", operaciones[i]);
        imprimirBinario(cinta2, tam);  // Imprimir cinta2
        printf(" = ");
        imprimirBinario(mt.resultado, RESULT_TAPE_SIZE);  // Imprimir todo el resultado
        printf("\n\n");
    }

    return 0;
}
