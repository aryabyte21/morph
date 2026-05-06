def greet(name):
    print("hi", name)
    if len(name) > 0:
        print(name.upper())
    log.info("done")

x = print
greet("world")
print(x)
