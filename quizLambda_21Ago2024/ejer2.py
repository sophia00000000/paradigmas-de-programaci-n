palabras = ["perro", "gato", "elefante", "oso", "jirafa"]
largas = list(filter(lambda palabra: len(palabra)>5, palabras))
print(largas)  