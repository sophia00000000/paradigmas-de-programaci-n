class Estado:
    Q0 = "q0"  # Estado inicial
    Q1 = "q1"  # Leer bits de las dos cintas
    Q2 = "q2"  # Manejar acarreo o préstamo
    Q3 = "q3"  # Escribir el resultado en la cinta
    Q4 = "q4"  # Mover cabezales
    Q5 = "q5"  # Estado final


class Cinta:
    def __init__(self, tamano):
        self.cinta = [0] * tamano
        self.cabezal = tamano - 1

    def leer(self):
        return self.cinta[self.cabezal] if self.cabezal >= 0 else 0

    def escribir(self, valor):
        if self.cabezal >= 0:
            self.cinta[self.cabezal] = valor

    def mover_izquierda(self):
        self.cabezal -= 1

    def imprimir(self):
        inicio = next((i for i, bit in enumerate(self.cinta) if bit != 0), len(self.cinta))
        return ''.join(map(str, self.cinta[inicio:])) if inicio != len(self.cinta) else '0'


class MaquinaTuring:
    MAX_TAPE_SIZE = 100
    RESULT_TAPE_SIZE = MAX_TAPE_SIZE * 2

    def __init__(self, cinta1, cinta2, operacion):
        self.cinta1 = Cinta(len(cinta1))
        self.cinta2 = Cinta(len(cinta2))
        self.resultado = Cinta(self.RESULT_TAPE_SIZE)
        self.cinta1.cinta = cinta1[:]
        self.cinta2.cinta = cinta2[:]
        self.operacion = operacion
        self.estado = Estado.Q0
        self.acarreo = 0
        self.prestamo = 0

    def transicion(self):
        if self.estado == Estado.Q0:
            self.estado = Estado.Q1

        elif self.estado == Estado.Q1:
            if self.cinta1.cabezal >= 0 or self.cinta2.cabezal >= 0 or self.acarreo > 0:
                self.estado = Estado.Q2
            else:
                self.estado = Estado.Q5

        elif self.estado == Estado.Q2:
            bit1 = self.cinta1.leer()
            bit2 = self.cinta2.leer()

            if self.operacion == '+':
                suma = bit1 + bit2 + self.acarreo
                if suma == 0 or suma == 1:
                    self.resultado.escribir(suma)
                    self.acarreo = 0
                elif suma == 2:
                    self.resultado.escribir(0)
                    self.acarreo = 1
                elif suma == 3:
                    self.resultado.escribir(1)
                    self.acarreo = 1

            elif self.operacion == '-':
                bit1 -= self.prestamo
                if bit1 >= bit2:
                    self.resultado.escribir(bit1 - bit2)
                    self.prestamo = 0
                else:
                    self.resultado.escribir((bit1 + 2) - bit2)
                    self.prestamo = 1

            self.estado = Estado.Q3

        elif self.estado == Estado.Q3:
            self.estado = Estado.Q4

        elif self.estado == Estado.Q4:
            self.cinta1.mover_izquierda()
            self.cinta2.mover_izquierda()
            self.resultado.mover_izquierda()
            self.estado = Estado.Q1

    def ejecutar(self):
        while self.estado != Estado.Q5:
            self.transicion()

    def imprimir_resultado(self):
        return self.resultado.imprimir()


# Función para probar la máquina de Turing
def probar_maquina(cinta1, cinta2):
    operaciones = ['+', '-']
    nombres_operaciones = ["suma", "resta"]

    for i in range(2):
        mt = MaquinaTuring(cinta1, cinta2, operaciones[i])
        mt.ejecutar()
        print(f"Resultado de la {nombres_operaciones[i]}:")
        print(f"{''.join(map(str, cinta1))} {operaciones[i]} {''.join(map(str, cinta2))} = {mt.imprimir_resultado()}")
        print()


# Prueba
if __name__ == "__main__":
    cinta1 = [1, 0, 1, 1]  # 11 en binario
    cinta2 = [0, 1, 0, 0]  # 4 en binario
    probar_maquina(cinta1, cinta2)
