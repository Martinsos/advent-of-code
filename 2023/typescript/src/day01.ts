import * as fs from "fs";
import _ from "lodash";

day01();

function day01(): void {
  const lines: string[] = fs
    .readFileSync("data/day01-input.txt", { encoding: "utf8" })
    .trim()
    .split("\n");
  console.log("Part one:", _.sum(lines.map(findCalibrationNumber)));
  // Part two is off by just a bit (5) and I can't figure out why.
  console.log("Part two:", _.sum(lines.map(spellingsToDigits).map(findCalibrationNumber)));
}

function findCalibrationNumber(line: string): number {
  const digits: string[] = [...line].filter((c) => c >= "1" && c <= "9");
  return parseInt(digits[0] + _.last(digits));
}

function spellingsToDigits(line: string): string {
  if (line.length === 0) return line;
  const spellings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];
  for (let i = 0; i < spellings.length; i++) {
    const spelling = spellings[i];
    if (line.startsWith(spelling)) {
      return (i + 1).toString() + spellingsToDigits(line.slice(spelling.length));
    }
  }
  return line[0] + spellingsToDigits(line.slice(1));
}
