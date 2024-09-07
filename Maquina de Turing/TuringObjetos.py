from enum import Enum
from typing import List

class Estado(Enum):
    q0 = 0  # Estado inicial
    q1 = 1  # Leyendo primera cinta
    q2 = 2  # Leyendo segunda cinta
    q3 = 3  # Eligiendo operación
    q4 = 4  # Realizando operación
    q5 = 5  # Estado final

class MaquinaTuring:
    MAX_TAPE_SIZE = 100
    RESULT_TAPE_SIZE = MAX_TAPE_SIZE * 2  # Doble tamaño para el resultado

    def __init__(self, cinta1: List[int], cinta2: List[int], operacion: str):
        self.cinta1 = cinta1[:]
        self.cinta2 = cinta2[:]
        self.resultado = [0] * self.RESULT_TAPE_SIZE
        self.cabezal1 = 0
        self.cabezal2 = 0
        self.tam_entrada = len(cinta1)
        self.tam_resultado = self.RESULT_TAPE_SIZE
        self.estado = Estado.q0
        self.operacion = operacion

    @staticmethod
    def binario_a_decimal(binario: List[int]) -> int:
        return int(''.join(map(str, binario)), 2)

    @staticmethod
    def decimal_a_binario(decimal: int, tam: int) -> List[int]:
        return [int(b) for b in format(decimal, f'0{tam}b')]

    def realizar_operacion(self):
        num1 = self.binario_a_decimal(self.cinta1)
        num2 = self.binario_a_decimal(self.cinta2)

        if self.operacion == '+':
            resultado = num1 + num2
        elif self.operacion == '-':
            resultado = max(num1 - num2, 0)
        elif self.operacion == '*':
            resultado = num1 * num2
        elif self.operacion == '/':
            if num2 != 0:
                resultado = num1 // num2
            else:
                print("Error: División por cero.")
                return
        else:
            print("Operador no válido.")
            return

        self.resultado = self.decimal_a_binario(resultado, self.tam_resultado)

    def transicion(self):
        if self.estado == Estado.q0:
            self.estado = Estado.q1
        elif self.estado == Estado.q1:
            if self.cabezal1 < self.tam_entrada - 1:
                self.cabezal1 += 1
            else:
                self.estado = Estado.q2
        elif self.estado == Estado.q2:
            if self.cabezal2 < self.tam_entrada - 1:
                self.cabezal2 += 1
            else:
                self.estado = Estado.q3
        elif self.estado == Estado.q3:
            self.estado = Estado.q4
        elif self.estado == Estado.q4:
            self.realizar_operacion()
            self.estado = Estado.q5

    def ejecutar(self):
        while self.estado != Estado.q5:
            self.transicion()

    @staticmethod
    def imprimir_binario(binario: List[int]):
        inicio = next((i for i, bit in enumerate(binario) if bit == 1), len(binario))
        if inicio == len(binario):
            print("0", end="")
        else:
            print(''.join(map(str, binario[inicio:])), end="")

def main():
    cinta1 = [1, 1, 1, 1, 0, 0, 0]  # 12 en decimal
    cinta2 = [0, 1, 0, 0, 1, 0, 0]  # 6 en decimal

    print("Número 1: ", end="")
    MaquinaTuring.imprimir_binario(cinta1)
    print("\nNúmero 2: ", end="")
    MaquinaTuring.imprimir_binario(cinta2)
    print("\n")

    operaciones = ['+', '-', '*', '/']
    nombres_operaciones = ["suma", "resta", "multiplicación", "división"]

    for op, nombre in zip(operaciones, nombres_operaciones):
        mt = MaquinaTuring(cinta1, cinta2, op)
        mt.ejecutar()

        print(f"Resultado de la {nombre}:")
        MaquinaTuring.imprimir_binario(cinta1)
        print(f" {op} ", end="")
        MaquinaTuring.imprimir_binario(cinta2)
        print(" = ", end="")
        MaquinaTuring.imprimir_binario(mt.resultado)
        print("\n")

if __name__ == "__main__":
    main()
