function greet(name: string) {
  console.log("hi", name);
  if (name.length > 0) {
    console.log(name.toUpperCase());
  }
  logger.info("done");
}

const x = console.log;
greet("world");
