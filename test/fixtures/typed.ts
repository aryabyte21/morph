function log(msg: string | number): void {
  console.log(msg);
}

const greeting: string = "hello";
const count: number = 42;
const enabled: boolean = true;

function caller(s: string, n: number, b: boolean) {
  log(s);
  log(n);
  log("inline string");
  log(123);
  log(greeting);
  log(count);
  log(enabled as any);
}
