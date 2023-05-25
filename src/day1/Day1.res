let input = Node.Fs.readFileAsUtf8Sync("./input.txt")

// part 1
let splitByElf = input => input->Js.String2.split("\n\n")

// calculate calorie of each elf
let splitByItem = input => input->Js.String2.split("\n")
let parseToInt = items => items->Array.keepMap(Int.fromString)
let sumCalories = calories => calories->Array.reduce(0, (acc, val) => acc + val)

input
->splitByElf
->Array.map(splitByItem)
->Array.map(parseToInt)
->Array.map(sumCalories)
->Js.Math.maxMany_int
->Js.log

// part 2

let getTopThree = elves =>
  elves->List.fromArray->List.sort((a, b) => b - a)->List.toArray->Array.slice(~offset=0, ~len=3)

input
->splitByElf
->Array.map(splitByItem)
->Array.map(parseToInt)
->Array.map(sumCalories)
->getTopThree
->sumCalories
->Js.log
