let input = Node.Fs.readFileAsUtf8Sync("./input.txt")

// part 1
let splitRucksacks = input => input->Js.String2.split("\n")

let splitToItemArray = input => input->Js.String2.split("")

let splitToCompartmentsSet = input => {
  let compartmentLength = input->Array.length / 2
  (
    input->Array.slice(~offset=0, ~len=compartmentLength)->Set.String.fromArray,
    input->Array.sliceToEnd(compartmentLength)->Set.String.fromArray,
  )
}

let getCommonItem = compartments => {
  let (compartment1, compartment2) = compartments
  compartment1->Set.String.intersect(compartment2)->Set.String.toArray
}

let getPriority = item => {
  let charCode = item->Js.String2.charCodeAt(0)->Float.toInt
  if charCode > 96 {
    charCode - 96
  } else {
    charCode - 38
  }
}

let getSum = input => input->Array.reduce(0, (acc, val) => acc + val)

let getTotalPoint = input =>
  input
  ->Array.map(rucksacks => rucksacks->Array.map(getPriority))
  ->Array.map(rucksacks => rucksacks->getSum)
  ->getSum

input
->splitRucksacks
->Array.map(splitToItemArray)
->Array.map(splitToCompartmentsSet)
->Array.map(getCommonItem)
->getTotalPoint
->Js.log

// part 2
let splitToGroups = (input: array<string>, ~size) =>
  input
  ->Array.reduce(list{list{}}, (acc, val) => {
    let head = acc->List.headExn // The initial value of reduce has List.head.

    head->List.length > size
      ? acc->List.add(list{val})
      : acc
        ->List.tail
        ->Option.mapWithDefault(list{head->List.add(val)}, tail' =>
          tail'->List.add(head->List.add(val))
        )
  })
  ->List.toArray

let parseToItemSet = rucksack => rucksack->Js.String2.split("")->Set.String.fromArray

let getBadges = group =>
  group
  ->List.head
  ->Option.map(head' => group->List.reduce(head', (acc, val) => acc->Set.String.intersect(val)))
  ->Option.map(badges' => badges'->Set.String.toArray)

input
->splitRucksacks
->splitToGroups(~size=2)
->Array.map(group => group->List.map(parseToItemSet))
->Array.keepMap(getBadges)
->getTotalPoint
->Js.log

// 가위바위보 => function and inverse funciton
