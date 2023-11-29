import * as fs from "fs";

async function day01(): Promise<void> {
  const inputText: string = fs.readFileSync("data/day01-input.txt", { encoding: "utf8" });
  console.log(inputText);
}

day01();
