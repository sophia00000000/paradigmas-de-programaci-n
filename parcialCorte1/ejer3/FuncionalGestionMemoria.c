#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Definición de la estructura optimizada para almacenar la información del estudiante
typedef struct {
    char *nombre;
    char *apellido;
    unsigned int edad : 7;  // Edad almacenada usando solo 7 bits (0-127)
    char ID[9];             // Almacenamos ID como un array de 8 caracteres + 1 para el terminador null
    int *calificaciones;
    size_t num_calificaciones;
} Estudiante;

// Función para crear un nuevo estudiante
Estudiante *crear_estudiante(const char *nombre, const char *apellido, unsigned int edad, const char *ID, int *calificaciones, size_t num_calificaciones) {
    Estudiante *nuevo_estudiante = (Estudiante *)malloc(sizeof(Estudiante));

    if (nuevo_estudiante == NULL) {
        printf("Error al asignar memoria para el estudiante.\n");
        return NULL;
    }

    nuevo_estudiante->nombre = (char *)malloc(strlen(nombre) + 1);
    nuevo_estudiante->apellido = (char *)malloc(strlen(apellido) + 1);

    if (nuevo_estudiante->nombre == NULL || nuevo_estudiante->apellido == NULL) {
        printf("Error al asignar memoria para los campos de texto.\n");
        free(nuevo_estudiante);
        return NULL;
    }

    strcpy(nuevo_estudiante->nombre, nombre);
    strcpy(nuevo_estudiante->apellido, apellido);
    strncpy(nuevo_estudiante->ID, ID, 8);  // Copiar los primeros 8 caracteres de ID y asegurarse de que esté terminado en '\0'
    nuevo_estudiante->ID[8] = '\0';

    nuevo_estudiante->edad = edad;

    nuevo_estudiante->calificaciones = (int *)malloc(num_calificaciones * sizeof(int));
    if (nuevo_estudiante->calificaciones == NULL) {
        printf("Error al asignar memoria para las calificaciones.\n");
        free(nuevo_estudiante->nombre);
        free(nuevo_estudiante->apellido);
        free(nuevo_estudiante);
        return NULL;
    }

    for (size_t i = 0; i < num_calificaciones; i++) {
        nuevo_estudiante->calificaciones[i] = calificaciones[i];
    }

    nuevo_estudiante->num_calificaciones = num_calificaciones;

    return nuevo_estudiante;
}

// Función para liberar la memoria de un estudiante
void liberar_estudiante(Estudiante *estudiante) {
    if (estudiante != NULL) {
        free(estudiante->nombre);
        free(estudiante->apellido);
        free(estudiante->calificaciones);
        free(estudiante);
    }
}

// Función para imprimir la información de un estudiante
void imprimir_estudiante(const Estudiante *estudiante) {
    if (estudiante != NULL) {
        printf("Nombre: %s %s\n", estudiante->nombre, estudiante->apellido);
        printf("Edad: %u\n", estudiante->edad);
        printf("ID: %s\n", estudiante->ID);
        printf("Calificaciones: ");
        for (size_t i = 0; i < estudiante->num_calificaciones; i++) {
            printf("%d ", estudiante->calificaciones[i]);
        }
        printf("\n");
    }
}

// Función para calcular el uso de memoria de un estudiante
size_t calcular_memoria_usada(const Estudiante *estudiante) {
    return sizeof(Estudiante) +
           strlen(estudiante->nombre) + 1 +
           strlen(estudiante->apellido) + 1 +
           estudiante->num_calificaciones * sizeof(int);
}

// Función principal para probar el programa
int main() {
    int calificaciones[] = {85, 90, 78};
    size_t num_calificaciones = sizeof(calificaciones) / sizeof(calificaciones[0]);

    Estudiante *estudiante = crear_estudiante("Carlos", "Gomez", 20, "12345678", calificaciones, num_calificaciones);

    if (estudiante != NULL) {
        imprimir_estudiante(estudiante);

        // Calcular y mostrar el uso de memoria
        size_t memoria_usada = calcular_memoria_usada(estudiante);
        printf("Memoria utilizada: %zu bytes\n", memoria_usada);

        // Liberar memoria
        liberar_estudiante(estudiante);
    }

    return 0;
}
