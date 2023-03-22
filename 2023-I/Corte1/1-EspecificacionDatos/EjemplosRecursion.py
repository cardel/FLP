"""
Carlos A Delgado S
22 de Marzo de 2023
Ejemplo de recursión
"""

def fibunnaci_cabeza(n):
    if n==0:
        return 1
    elif n==1:
        return 1
    else:
        return fibunnaci_cabeza(n-1)+fibunnaci_cabeza(n-2)
    
print(fibunnaci_cabeza(6))

def fibonacci_cola(n, a=1, b=1):
  if n == 0:
      return a
  elif n == 1:
      return b
  else:
      yield from fibonacci_cola(n-1, b, a+b)

print(fibonacci_cola(6))
