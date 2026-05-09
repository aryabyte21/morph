import { db } from "./db";

export async function createUser(name: string, email: string) {
  console.log("creating user", name);
  const id = crypto.randomUUID();
  await db.users.insert({ id, name, email });
  console.log("inserted", id);
  return id;
}

export async function deleteUser(id: number) {
  console.log("deleting", id);
  await db.users.delete({ id });
}

export function debug(x: number) {
  console.log(x);
}
