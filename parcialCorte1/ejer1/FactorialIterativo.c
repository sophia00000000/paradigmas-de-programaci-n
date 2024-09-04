#include <stdio.h>
#include <time.h>

int factorialIterativo(int n) {
    int r = 1;
    for (int i = n; i > 0; i--) {
        r *= i;
    }
    return r;
}



int main() {
    clock_t begin = clock();
    
    int number = 12; 
    int result = factorialIterativo(number);
    printf("El factorial iterativo de %d es %d\n", number, result);
    
    clock_t end = clock();
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("Tiempo de ejecución: %f segundos\n", time_spent);
    return 0;
}
