with open("input_data") as file:
    data = file.read().strip()

WIDTH = 25
HEIGHT = 6
RES = WIDTH * HEIGHT

layers = [data[i:i + RES] for i in range(0, len(data), RES)]

assert any("0" in layer for layer in layers)

smallest = min(layers, key=(lambda layer: layer.count("0")))
print(smallest.count("1") * smallest.count("2"))
