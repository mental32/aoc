with open("input_data") as file:
    modules = [int(line.strip()) for line in file if line]

raw_fuel_requirement = sum({(weight // 3) - 2 for weight in modules})

# Part 2

def calculate(weight: int) -> int:
    if (amount := (weight // 3) - 2) <= 0:
        return 0

    return amount + calculate(amount)

fuel_requirement = sum({calculate(weight) for weight in modules})
