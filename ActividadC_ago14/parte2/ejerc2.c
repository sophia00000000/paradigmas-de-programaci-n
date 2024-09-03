#include <stdio.h>

int main()
{
    char contra[8]="12345678";
    char input[8];
    printf("Ingrese contraseña");
    scanf("%s",input);
    for (int i = 0; i < 8; i++) {
        if (input[i] != contra[i]) {
            printf("Contraseña incorrecta\n");
            return 1;
        }
    }
    printf("Contraseña correcta\n");
    return 0;
}