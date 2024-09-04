#include <stdio.h>
#include <time.h>

unsigned long long memo[50]; // Ajustar el tamaño según sea necesario

unsigned long long factorial_memoization(int n) {
    if (n == 0 || n == 1)
        return 1;
    if (memo[n] != 0)
        return memo[n];
    memo[n] = n * factorial_memoization(n - 1);
    return memo[n];
}

int main() {

    clock_t begin = clock();
    
    int number = 12;
    unsigned long long result = factorial_memoization(number);
    printf("Factorial dinamico de %d es %llu\n", number, factorial_memoization(number));
    
    clock_t end = clock();
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("Tiempo de ejecución: %f segundos\n", time_spent);
    
    return 0;
}
