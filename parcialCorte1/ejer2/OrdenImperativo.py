def bubble_sort(estudiantes):
    n = len(estudiantes)
    for i in range(n):
        for j in range(0, n-i-1):
            # Comparamos las calificaciones primero
            if estudiantes[j][1] < estudiantes[j+1][1]:
                estudiantes[j], estudiantes[j+1] = estudiantes[j+1], estudiantes[j]
            # Si las calificaciones son iguales, ordenamos por nombre
            elif estudiantes[j][1] == estudiantes[j+1][1] and estudiantes[j][0] > estudiantes[j+1][0]:
                estudiantes[j], estudiantes[j+1] = estudiantes[j+1], estudiantes[j]

estudiantes = [
    ("Laura", 1),
    ("Angie", 4.5),
    ("Belen", 3.4),
    ("Carlos", 4),
    ("Nicolas", 5),
    ("Esteban", 1.2)
]

print("Lista antes del ordenamiento:")
for estudiante in estudiantes:
    print(estudiante)

bubble_sort(estudiantes)

print("\nLista después del ordenamiento:")
for estudiante in estudiantes:
    print(estudiante)
