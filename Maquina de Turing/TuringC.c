#include <stdio.h>
#include <string.h>

#define MAX_TAPE_SIZE 100
#define RESULT_TAPE_SIZE (MAX_TAPE_SIZE * 2)  // Doble tamaño para el resultado

typedef enum {
    q0, // Estado inicial
    q1, // Leyendo primera cinta
    q2, // Leyendo segunda cinta
    q3, // Eligiendo operación
    q4, // Realizando operación
    q5, // Estado final
} Estado;

typedef struct {
    int cinta1[MAX_TAPE_SIZE];
    int cinta2[MAX_TAPE_SIZE];
    int resultado[RESULT_TAPE_SIZE];
    int cabezal1;
    int cabezal2;
    int tam_entrada;
    int tam_resultado;
    Estado estado;
    char operacion;
} MaquinaTuring;

void inicializarMT(MaquinaTuring *mt, int cinta1[], int cinta2[], int tam, char op) {
    memcpy(mt->cinta1, cinta1, tam * sizeof(int));
    memcpy(mt->cinta2, cinta2, tam * sizeof(int));
    memset(mt->resultado, 0, RESULT_TAPE_SIZE * sizeof(int));
    mt->cabezal1 = 0;
    mt->cabezal2 = 0;
    mt->tam_entrada = tam;
    mt->tam_resultado = RESULT_TAPE_SIZE;
    mt->estado = q0;
    mt->operacion = op;
}

int binarioADecimal(int binario[], int tam) {
    int decimal = 0;
    for (int i = 0; i < tam; i++) {
        decimal = decimal * 2 + binario[i];
    }
    return decimal;
}

void decimalABinario(int decimal, int binario[], int tam) {
    for (int i = tam - 1; i >= 0; i--) {
        binario[i] = decimal % 2;
        decimal /= 2;
    }
}

void realizarOperacion(MaquinaTuring *mt) {
    int num1 = binarioADecimal(mt->cinta1, mt->tam_entrada);
    int num2 = binarioADecimal(mt->cinta2, mt->tam_entrada);
    int resultado;

    switch (mt->operacion) {
        case '+': resultado = num1 + num2; break;
        case '-': resultado = (num1 > num2) ? (num1 - num2) : 0; break;
        case '*': resultado = num1 * num2; break;
        case '/': 
            if (num2 != 0) resultado = num1 / num2;
            else {
                printf("Error: División por cero.\n");
                return;
            }
            break;
        default:
            printf("Operador no válido.\n");
            return;
    }

    decimalABinario(resultado, mt->resultado, mt->tam_resultado);
}

void transicion(MaquinaTuring *mt) {
    switch (mt->estado) {
        case q0:
            mt->estado = q1;
            break;
        case q1:
            if (mt->cabezal1 < mt->tam_entrada - 1) {
                mt->cabezal1++;
            } else {
                mt->estado = q2;
            }
            break;
        case q2:
            if (mt->cabezal2 < mt->tam_entrada - 1) {
                mt->cabezal2++;
            } else {
                mt->estado = q3;
            }
            break;
        case q3:
            mt->estado = q4;
            break;
        case q4:
            realizarOperacion(mt);
            mt->estado = q5;
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
    int cinta1[] = {1, 1, 1, 1, 0, 0, 0};  // 12 en decimal
    int cinta2[] = {0, 1, 0, 0, 1, 0, 0};  // 6 en decimal
    int tam = sizeof(cinta1) / sizeof(cinta1[0]);
    MaquinaTuring mt;

    printf("Número 1: ");
    imprimirBinario(cinta1, tam);
    printf("\nNúmero 2: ");
    imprimirBinario(cinta2, tam);
    printf("\n\n");

    char operaciones[] = {'+', '-', '*', '/'};
    char* nombres_operaciones[] = {"suma", "resta", "multiplicación", "división"};

    for (int i = 0; i < 4; i++) {
        inicializarMT(&mt, cinta1, cinta2, tam, operaciones[i]);
        ejecutarMT(&mt);

        printf("Resultado de la %s:\n", nombres_operaciones[i]);
        imprimirBinario(cinta1, tam);
        printf(" %c ", operaciones[i]);
        imprimirBinario(cinta2, tam);
        printf(" = ");
        imprimirBinario(mt.resultado, RESULT_TAPE_SIZE);
        printf("\n\n");
    }

    return 0;
}
