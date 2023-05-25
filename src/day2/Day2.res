let input = Node.Fs.readFileAsUtf8Sync("./input.txt")

type rock and paper and scissors

@deriving(jsConverter)
type rec rockPaperScissors = | @as(1) Rock | @as(2) Paper | @as(3) Scissors

@deriving(jsConverter)
type roundResult = | @as(6) Win | @as(3) Draw | @as(0) Lose

type roundInput = {
  draw: rockPaperScissors,
  win: rockPaperScissors,
  lose: rockPaperScissors,
}

type round = {
  opponent: roundInput,
  me: roundInput,
}

let rock = {
  draw: Rock,
  win: Scissors,
  lose: Paper,
}
let paper = {
  draw: Paper,
  win: Rock,
  lose: Scissors,
}
let scissors = {
  draw: Scissors,
  win: Paper,
  lose: Rock,
}

let splitRounds = input => input->Js.String2.split("\n")

let parseToRockPaperScissors = input =>
  switch input {
  | "A" | "X" => rock->Some
  | "B" | "Y" => paper->Some
  | "C" | "Z" => scissors->Some
  | _ => None
  }

let parseRoundInput = input => {
  switch input->Js.String2.split(" ") {
  | [opponent, me] =>
    switch (opponent->parseToRockPaperScissors, me->parseToRockPaperScissors) {
    | (Some(opponent'), Some(me')) =>
      Some({
        opponent: opponent',
        me: me',
      })
    | _ => None
    }
  | _ => None
  }
}

let getRoundPoint1 = roundInput => {
  let {opponent, me} = roundInput
  let result = if opponent.draw == me.win {
    Win
  } else if opponent.draw == me.draw {
    Draw
  } else {
    Lose
  }
  result->roundResultToJs + me.draw->rockPaperScissorsToJs
}

input
->splitRounds
->Array.keepMap(parseRoundInput)
->Array.map(getRoundPoint1)
->Array.reduce(0, (acc, val) => acc + val)
->Js.log

// part 2
let parseToResult = input =>
  switch input {
  | "X" => Lose->Some
  | "Y" => Draw->Some
  | "Z" => Win->Some
  | _ => None
  }

type roundInputAndResult = {
  opponent: roundInput,
  result: roundResult,
}

let parseRoundInputAndResult = input => {
  switch input->Js.String2.split(" ") {
  | [opponent, result] =>
    switch (opponent->parseToRockPaperScissors, result->parseToResult) {
    | (Some(opponent'), Some(result')) =>
      Some({
        opponent: opponent',
        result: result',
      })
    | _ => None
    }
  | _ => None
  }
}

let getMyInput = roundInputAndResult => {
  let {opponent, result} = roundInputAndResult
  switch result {
  | Win => opponent.lose
  | Draw => opponent.draw
  | Lose => opponent.win
  }
}

let getRoundPoint2 = roundInputAndResult => {
  roundInputAndResult.result->roundResultToJs +
    roundInputAndResult->getMyInput->rockPaperScissorsToJs
}

input
->splitRounds
->Array.keepMap(parseRoundInputAndResult)
->Array.map(getRoundPoint2)
->Array.reduce(0, (acc, val) => acc + val)
->Js.log
