magic = 20201227
value = key = 1

while True:
    value = (value * 7) % magic
    key = (key * 14505727) % magic

    if value == 16616892:
        print(key)
        break
