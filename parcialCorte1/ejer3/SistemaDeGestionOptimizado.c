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

// Definición de la estructura para la lista dinámica de estudiantes
typedef struct {
    Estudiante *estudiantes;
    size_t num_estudiantes;
} SistemaEstudiantes;

// Función para calcular el uso de memoria
size_t calcular_memoria_utilizada(const SistemaEstudiantes *sistema) {
    size_t memoria_total = sizeof(SistemaEstudiantes);

    for (size_t i = 0; i < sistema->num_estudiantes; i++) {
        Estudiante *estudiante = &sistema->estudiantes[i];
        memoria_total += sizeof(Estudiante);  // Memoria de la estructura principal
        memoria_total += strlen(estudiante->nombre) + 1;  // Memoria para el nombre
        memoria_total += strlen(estudiante->apellido) + 1;  // Memoria para el apellido
        memoria_total += estudiante->num_calificaciones * sizeof(int);  // Memoria para las calificaciones
    }

    return memoria_total;
}

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
    }
}

// Función para agregar un estudiante al sistema
void agregar_estudiante(SistemaEstudiantes *sistema, const char *nombre, const char *apellido, unsigned int edad, const char *ID, int *calificaciones, size_t num_calificaciones) {
    sistema->estudiantes = (Estudiante *)realloc(sistema->estudiantes, (sistema->num_estudiantes + 1) * sizeof(Estudiante));
    
    if (sistema->estudiantes == NULL) {
        printf("Error al redimensionar la memoria para los estudiantes.\n");
        return;
    }

    sistema->estudiantes[sistema->num_estudiantes] = *crear_estudiante(nombre, apellido, edad, ID, calificaciones, num_calificaciones);
    sistema->num_estudiantes++;

    printf("Estudiante \"%s %s\" agregado correctamente. Memoria utilizada: %zu bytes.\n", nombre, apellido, calcular_memoria_utilizada(sistema));
}

// Función para buscar un estudiante por ID
int buscar_estudiante(SistemaEstudiantes *sistema, const char *ID) {
    for (size_t i = 0; i < sistema->num_estudiantes; i++) {
        if (strcmp(sistema->estudiantes[i].ID, ID) == 0) {
            return i;
        }
    }
    return -1;  // No se encontró
}

// Función para actualizar la información de un estudiante
void actualizar_estudiante(SistemaEstudiantes *sistema, const char *ID, const char *nuevo_nombre, const char *nuevo_apellido, unsigned int nueva_edad) {
    int indice = buscar_estudiante(sistema, ID);
    
    if (indice == -1) {
        printf("Estudiante con ID \"%s\" no encontrado.\n", ID);
        return;
    }

    Estudiante *estudiante = &sistema->estudiantes[indice];

    free(estudiante->nombre);
    free(estudiante->apellido);

    estudiante->nombre = (char *)malloc(strlen(nuevo_nombre) + 1);
    estudiante->apellido = (char *)malloc(strlen(nuevo_apellido) + 1);
    
    strcpy(estudiante->nombre, nuevo_nombre);
    strcpy(estudiante->apellido, nuevo_apellido);
    estudiante->edad = nueva_edad;

    printf("Estudiante con ID \"%s\" actualizado correctamente. Memoria utilizada: %zu bytes.\n", ID, calcular_memoria_utilizada(sistema));
}

// Función para eliminar un estudiante
void eliminar_estudiante(SistemaEstudiantes *sistema, const char *ID) {
    int indice = buscar_estudiante(sistema, ID);
    
    if (indice == -1) {
        printf("Estudiante con ID \"%s\" no encontrado.\n", ID);
        return;
    }

    // Mover los estudiantes restantes para llenar el hueco
    for (size_t i = indice; i < sistema->num_estudiantes - 1; i++) {
        sistema->estudiantes[i] = sistema->estudiantes[i + 1];
    }

    // Reducir el tamaño del array de estudiantes
    Estudiante *nueva_lista = (Estudiante *)realloc(sistema->estudiantes, (sistema->num_estudiantes - 1) * sizeof(Estudiante));
    if (nueva_lista == NULL && sistema->num_estudiantes > 1) {
        printf("Error al reasignar memoria al eliminar estudiante.\n");
        return;
    }

    sistema->estudiantes = nueva_lista;
    sistema->num_estudiantes--;

    printf("Estudiante con ID \"%s\" eliminado correctamente.\n", ID);
    printf("Memoria utilizada: %zu bytes.\n", calcular_memoria_utilizada(sistema));
}

// Función para imprimir la información de todos los estudiantes
void imprimir_estudiantes(const SistemaEstudiantes *sistema) {
    printf("Estudiantes en el sistema:\n");
    for (size_t i = 0; i < sistema->num_estudiantes; i++) {
        printf("Nombre: %s %s, Edad: %u, ID: %s\n", sistema->estudiantes[i].nombre, sistema->estudiantes[i].apellido, sistema->estudiantes[i].edad, sistema->estudiantes[i].ID);
    }
}

int main() {
    SistemaEstudiantes sistema = {NULL, 0};

    int calificaciones1[] = {85, 90, 78};
    int calificaciones2[] = {88, 92, 75};

    agregar_estudiante(&sistema, "Carlos", "Gomez", 20, "12345678", calificaciones1, 3);
    agregar_estudiante(&sistema, "Ana", "Martinez", 21, "87654321", calificaciones2, 3);

    imprimir_estudiantes(&sistema);

    actualizar_estudiante(&sistema, "12345678", "Carlos Alberto", "Gomez Perez", 21);
    imprimir_estudiantes(&sistema);

    eliminar_estudiante(&sistema, "87654321");
    imprimir_estudiantes(&sistema);

    // Bucle para liberar todos los estudiantes al final
    while (sistema.num_estudiantes > 0) {
        size_t memoria_antes = calcular_memoria_utilizada(&sistema);
        eliminar_estudiante(&sistema, sistema.estudiantes[0].ID);
        size_t memoria_despues = calcular_memoria_utilizada(&sistema);
        printf("Memoria liberada: %zu bytes\n", memoria_antes - memoria_despues);
    }

    return 0;
}
