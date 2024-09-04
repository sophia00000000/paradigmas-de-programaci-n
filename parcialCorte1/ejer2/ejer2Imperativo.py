def bubble_sort(students):
    n = len(students)
    for i in range(n):
        for j in range(0, n-i-1):
            # Comparamos las calificaciones primero
            if students[j][1] < students[j+1][1]:
                students[j], students[j+1] = students[j+1], students[j]
            # Si las calificaciones son iguales, ordenamos por nombre
            elif students[j][1] == students[j+1][1] and students[j][0] > students[j+1][0]:
                students[j], students[j+1] = students[j+1], students[j]

students = [
    ("Laura", 1),
    ("Angie", 4.5),
    ("Belen", 3.4),
    ("Carlos", 4),
    ("Nicolas", 5),
    ("Esteban", 1.2)
]

print("Lista antes del ordenamiento:")
for student in students:
    print(student)

bubble_sort(students)

print("\nLista después del ordenamiento:")
for student in students:
    print(student)
