from sympy import symbols
from sympy.solvers.diophantine.diophantine import diop_linear

# The idea is to write X = a_0 + 10a_1 + 100a_2 + ... + 10^n a_n.
# This allows us to reformulate 99X = 1X1 as the linear diophantine equation,
# 89a_0 + 890a_1 + 8900a_2 + ... + 89*10^n*a_n - 10^(n+2)+1 = 0,
# which can be solved using the sympy library.

# get the equation for n digits
def diophanineEquation(n):
    equation = 89*symbols("a0")
    for i in range(n-1):
        equation += 89*10**(i+1)*symbols(f"a{i+1}")

    equation = equation-10**(n)-1
    return equation

# find the solutions to the diophantine equation
def findNumber(digits):
    equation = diophanineEquation(digits)
    solutions = diop_linear(equation)
    if any(solutions):
        solution = int(str(solutions[0]).split(" + ")[1])
        return solution
    else:
        return None

# go through the first 100 digits and look for solutions
if __name__ == "__main__":
    for i in range(200):
        number = findNumber(i)
        if number:
            print(f"Found solution with {i} digits: {number}")
        else:
            print(f"No solution with {i} digits exists.")