from blessings import Terminal

with open("input_data") as file:
    data = file.read().strip()

WIDTH = 25
HEIGHT = 6
RES = WIDTH * HEIGHT

layers = [data[i:i + RES] for i in range(0, len(data), RES)]

# Part 1

assert any("0" in layer for layer in layers)

smallest = min(layers, key=(lambda layer: layer.count("0")))
print(smallest.count("1") * smallest.count("2"))

# Part 2

terminal = Terminal()

with terminal.fullscreen():
    print(terminal.clear)
    for y in range(HEIGHT):
        for x in range(WIDTH):
            pixel = next(layer[y * WIDTH + x] for layer in layers if layer[y * WIDTH + x] != "2")

            with terminal.location(x, y):
                if pixel == "1":
                    print(terminal.on_white(" "), end="", flush=True)
                else:
                    print(terminal.on_black(" "), end="", flush=True)

        print("", flush=True, end="")

