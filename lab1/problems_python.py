#2
def problem2_iterative():
    """Итеративное решение с циклом while"""
    limit = 4_000_000
    total = 0
    a, b = 1, 2
    while a <= limit:
        if a % 2 == 0:
            total += a
        a, b = b, a + b
    return total


def problem2_recursive():
    """Рекурсивное решение (не хвостовая рекурсия)"""
    limit = 4_000_000
    
    def sum_even_fib(a, b):
        if a > limit:
            return 0
        elif a % 2 == 0:
            return a + sum_even_fib(b, a + b)
        else:
            return sum_even_fib(b, a + b)
    
    return sum_even_fib(1, 2)


def problem2_functional():
    """Функциональный стиль с генератором и функциями высшего порядка"""
    limit = 4_000_000
    
    # Генератор чисел Фибоначчи
    def fibonacci_gen():
        a, b = 1, 2
        while True:
            yield a
            a, b = b, a + b
    
    # Берём числа до limit, фильтруем чётные, суммируем
    from itertools import takewhile
    fibs = takewhile(lambda x: x <= limit, fibonacci_gen())
    return sum(filter(lambda x: x % 2 == 0, fibs))


def problem2_comprehension():
    """С использованием list comprehension"""
    limit = 4_000_000
    
    # Генерируем список Фибоначчи
    fibs = []
    a, b = 1, 2
    while a <= limit:
        fibs.append(a)
        a, b = b, a + b
    
    return sum(f for f in fibs if f % 2 == 0)



#29
def problem29_iterative():
    """Итеративное решение с вложенными циклами"""
    distinct = set()
    for a in range(2, 101):
        for b in range(2, 101):
            distinct.add(a ** b)
    return len(distinct)


def problem29_recursive():
    """Рекурсивное решение (для меньшего диапазона, т.к. Python не оптимизирует рекурсию)"""
    import sys
    old_limit = sys.getrecursionlimit()
    sys.setrecursionlimit(15000)  # Увеличиваем лимит рекурсии
    
    def collect_powers(a, b, acc):
        if a > 100:
            return acc
        elif b > 100:
            return collect_powers(a + 1, 2, acc)
        else:
            acc.add(a ** b)
            return collect_powers(a, b + 1, acc)
    
    result = len(collect_powers(2, 2, set()))
    sys.setrecursionlimit(old_limit)
    return result


def problem29_functional():
    """Функциональный стиль с map и reduce"""
    from itertools import product
    
    # Генерация пар -> отображение в степени -> множество
    pairs = product(range(2, 101), range(2, 101))
    powers = map(lambda p: p[0] ** p[1], pairs)
    return len(set(powers))


def problem29_comprehension():
    """С использованием set comprehension"""
    return len({a ** b for a in range(2, 101) for b in range(2, 101)})


def problem29_generator():
    """С использованием генератора (ленивые вычисления)"""
    def power_gen():
        for a in range(2, 101):
            for b in range(2, 101):
                yield a ** b
    
    return len(set(power_gen()))


if __name__ == "__main__":
    print("=" * 60)
    print("Project Euler - Problem 2: Even Fibonacci Numbers")
    print("=" * 60)
    print(f"1. Итеративное:         {problem2_iterative()}")
    print(f"2. Рекурсивное:         {problem2_recursive()}")
    print(f"3. Функциональное:      {problem2_functional()}")
    print(f"4. List comprehension:  {problem2_comprehension()}")
    
    print()
    print("=" * 60)
    print("Project Euler - Problem 29: Distinct Powers")
    print("=" * 60)
    print(f"1. Итеративное:         {problem29_iterative()}")
    print(f"2. Рекурсивное:         {problem29_recursive()}")
    print(f"3. Функциональное:      {problem29_functional()}")
    print(f"4. Set comprehension:   {problem29_comprehension()}")
    print(f"5. С генератором:       {problem29_generator()}")
