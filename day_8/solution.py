with open("input_data") as file:
    data = file.read().strip()

WIDTH = 25
HEIGHT = 6
RES = WIDTH * HEIGHT

layers = [data[i:i + RES] for i in range(0, len(data), RES)]

assert any("0" in layer for layer in layers)

smallest = min(layers, key=(lambda layer: layer.count("0")))
print(smallest.count("1") * smallest.count("2"))

from blessings import Terminal

terminal = Terminal()

final = []

with terminal.fullscreen():
    print(terminal.clear)
    for y in range(HEIGHT):
        for x in range(WIDTH):
            for layer in layers:
                pixel = layer[y * WIDTH + x]
                if pixel == "2":
                    # Transparent
                    continue

                with terminal.location(x, y):
                    if pixel == "1":
                        print("*", end="", flush=True)
                    else:
                        print(" ", end="", flush=True)
                break
        print("", flush=True, end="")
    input()  # Needed as exit of fullscreen clears the screen.
