import * as fs from "fs";
import _ from "lodash";

day01();

function day01(): void {
  const lines: string[] = fs
    .readFileSync("data/day01-input.txt", { encoding: "utf8" })
    .trim()
    .split("\n");
  console.log("Part one:", _.sum(lines.map(findCalibrationNumber)));
  console.log("Part two:", _.sum(lines.map(spellingsToDigits).map(findCalibrationNumber)));
}

function findCalibrationNumber(line: string): number {
  const digits: string[] = [...line].filter((c) => c >= "1" && c <= "9");
  return parseInt(digits[0] + _.last(digits));
}

// TODO: There is a mistake here. We can't just transform all spellings to digits
// from left to right becaue if line ends with oneight then we thing it is 1ight
// but it should be on8, because when looking at last digit we need to do it from right.
// So we should look for first spelling from left and for last spelling from right.
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
