#include <stdio.h>
#include <time.h>


int factorialRecursivo(int n){
    if (n == 0) {
        return 1;
    } else {
        return n * factorialRecursivo(n - 1);
    }
}

int main() {
    clock_t begin = clock();

    int number = 12; 
    int result = factorialRecursivo(number);
    printf("El factorial recursivo de %d es %d\n", number, result);
    
    clock_t end = clock();
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("Tiempo de ejecución: %f segundos\n", time_spent);
  return 0;
}
