def log(msg: str) -> None:
    print(msg)

a: str = "hello"
b: int = 42

def caller(s: str, n: int):
    log(s)
    log("inline")
    log(a)
    log(b)
