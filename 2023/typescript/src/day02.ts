import * as fs from "fs";
import _ from "lodash";

day02();

function day02(): void {
  const lines: string[] = fs
    .readFileSync("data/day02-input.txt", { encoding: "utf8" })
    .trim()
    .split("\n");
  console.log(_.sum(lines.map((l, i) => (isGamePossible(parseGame(l)) ? i + 1 : 0))));
  console.log(_.sum(lines.map((l) => cubeSetPower(minSuperset(parseGame(l))))));
}

interface CubeSet {
  red: number;
  green: number;
  blue: number;
}

function newCubeSet({
  red = 0,
  green = 0,
  blue = 0,
}: {
  red?: number;
  green?: number;
  blue?: number;
}): CubeSet {
  return { red, green, blue };
}

function parseGame(line: string): CubeSet[] {
  return line.split(": ")[1].split("; ").map(parseCubeSet);
}

function parseCubeSet(str: string): CubeSet {
  return str.split(", ").map(parseCubeColorPick).reduce(concatCubeSets);
}

function parseCubeColorPick(str: string): CubeSet {
  const [numStr, color] = str.split(" ");
  const num = parseInt(numStr);
  switch (color) {
    case "red":
      return newCubeSet({ red: num });
    case "green":
      return newCubeSet({ green: num });
    case "blue":
      return newCubeSet({ blue: num });
    default:
      throw new Error("Invalid cube color");
  }
}

function concatCubeSets(cs1: CubeSet, cs2: CubeSet): CubeSet {
  return {
    red: cs1.red + cs2.red,
    green: cs1.green + cs2.green,
    blue: cs1.blue + cs2.blue,
  };
}

function isSubsetOf(cs1: CubeSet, cs2: CubeSet): boolean {
  return cs1.red <= cs2.red && cs1.green <= cs2.green && cs1.blue <= cs2.blue;
}

function isGamePossible(cubeSets: CubeSet[]): boolean {
  return cubeSets.every((cs) => isSubsetOf(cs, { red: 12, green: 13, blue: 14 }));
}

function minSuperset(cubeSets: CubeSet[]): CubeSet {
  return cubeSets.reduce((cb1, cb2) => ({
    red: Math.max(cb1.red, cb2.red),
    green: Math.max(cb1.green, cb2.green),
    blue: Math.max(cb1.blue, cb2.blue),
  }));
}

function cubeSetPower(cs: CubeSet): number {
  return cs.red * cs.green * cs.blue;
}
