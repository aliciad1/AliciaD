(* file: main.ml
  author: Bob muller

  CSCI 1103 Computer Science 1 Honors

   A sudoku solver.

  To run:

  > cd src
  > dune exec bin/main.exe inputfile
*)
let boardSize = ref 0

let displayWidth  = 800.0
let displayHeight = 800.0
let side = displayWidth /. 9.0
let empty = Image.empty displayWidth displayHeight
let emptySquare = Image.rectangle side side Color.gray
let digitSize = 50.0
let offset = (side -. digitSize) /. 1.5
let clockRate = 0.005

type entry = { n : int; fixed : bool }
type board = entry array array

let printBoard board =
  let n = Array.length board
  in
  for row = 0 to n - 1 do
    for col = 0 to n - 1 do
      Lib.pfmt "%d " board.(row).(col).n
    done ;
    print_string "\n"
  done ;
  print_string "\n"

type state = Paused | Running | Solved

let toggle state =
  match state with
  | Paused -> Running
  | Running | Solved -> Paused

type play = { row : int
            ; col : int
            ; number : int
            }

let play2String {row; col; number} =
  Lib.fmt "{row=%d; col=%d; number=%d}" row col number

let printPlay play = Lib.pfmt "%s\n" (play2String play)

type memory = play Stack.t

let printMemory memory =
  Lib.pfmt "memory=\n";
  Stack.iter printPlay memory

type model = { state  : state
             ; memory : memory
             ; board  : board
             }

let charToEntry a =
  { fixed = if a = '*' then false else true (* if a = '*' then false else true; originally "= true" *)
  ; n = if a = '*' then 0 else Char.code(a) - Char.code('0')
  }

(* getFileName : unit -> path *)
let getFilePath () =
  Lib.fmt "%s/%s" (Unix.getcwd ()) Sys.argv.(1)

let readBoard () =
  let filename = getFilePath () in
  let inch = open_in filename in
  let rec loop codeList =
    try
      let line = input_line inch in
      let row = Array.of_list (List.map charToEntry (Lib.explode line))
      in
      loop (row :: codeList)
    with
      End_of_file -> close_in inch ;
      Array.of_list (List.rev codeList)
  in
  boardSize := int_of_string (input_line inch);
  loop []

let initialModel = { state = Paused
                   ; memory = Stack.create ()
                   ; board = readBoard ()
                   }

(* Code related to viewing the model. ******************************
*)
let colorOf entry =
  match entry.fixed with
  | true  -> Color.black
  | false -> Color.dodgerBlue

(* boxOf : entry -> Image.t *)
let boxOf entry =
  let digit = string_of_int entry.n in
  let text = Image.text digit ~size:digitSize (colorOf entry)
  in
  match (1 <= entry.n && entry.n <= 9) with
  | true  -> Image.placeImage text (offset, offset) emptySquare
  | false -> emptySquare

(* view : model -> Image.t *)
let view { board } =
  let n = Array.length board in
  let rec loop row col image =
    let digitImage = boxOf board.(row).(col) in
    let x = side *. float col in
    let y = side *. float row in
    let newImage = Image.placeImage digitImage (x, y) image
    in
    match (row = (n - 1), col = (n - 1)) with
    | (true,  true)  -> newImage
    | (true,  false) -> loop row (col + 1) newImage
    | (false, true)  -> loop (row + 1) 0 newImage
    | (false, false) -> loop row (col + 1) newImage
  in
  loop 0 0 empty

(* Code related to updating the model. ******************************
 *
 * Four functions for checking if a number is acceptable in a spot.
*)
 let sectionOK number board i j = 
  let iRange = (3*(i/3)) in
  let jRange = (3*(j/3))
  in
  try
    for p = iRange to iRange + 2 do
      for q = jRange to jRange + 2 do
        let value = board.(p).(q)
      in
      match value.n == number with
      | true -> raise Exit
      | false -> ()
      done
    done;
    true
  with | Exit -> false
 
let rowOK number board i j =
  try
    for k = 0 to 8 do
      let value = board.(i).(k)
    in
      match value.n == number with
      | true -> raise Exit
      | false -> ()
    done;
    true
  with | Exit -> false
 
 
let colOK number board i j =
  try
    for k = 0 to 8 do
      let value = board.(k).(j)
    in
      match value.n == number with
      | true -> raise Exit
      | false -> ()
    done;
    true
  with | Exit -> false

let numberOK number board row col =
  rowOK number board row col &&
  colOK number board row col &&
  sectionOK number board row col


(*allows us to find first empty cell on the board; recursively looping on row and col under the constraints that the row and col no. stay less than 9 each...*)
let findFirstEmptyCell board = 
  let rec loop row col = 
    match (row < !boardSize, col < !boardSize) with
    | (false, _) -> None (* when moved past the 9th row and column position is outside the board; return None (which will indicate that the board has been solved)*)
    | (true, false) -> loop (row + 1) 0 (* when in an existing row, but ouside the 9th column, increment row to move to the next row and start at column 0 again *)
    | (true, true) -> 
      if board.(row).(col).n = 0 then (* when inside the 9x9 grid AND the value in this position is 0 (aka there is no given number present)*)
        Some (row,col) (* returns the index coordinates in the board *)
      else
        loop row (col + 1) (* else, loops to perform the same actions on the next number in the same row*)
  in
  loop 0 0 (* indicate to start process from the very first box *)

let rec startingWith number board row col =
  match number <= 9 with 
  | true -> 
    (match numberOK number board row col with
     | true  -> Some number
     | false -> startingWith (number + 1) board row col)
  | false -> None


  let rec backtrack board memory = 
    match Stack.is_empty memory with
    | true -> false
    | false ->
      let {row; col; number} = Stack.pop memory
     in
     match startingWith (number + 1) board row col with
     | Some number ->
       board.(row).(col) <- {fixed = false; n = number};
       Stack.push {row; col; number} memory;
       true
     | None ->
       board.(row).(col) <- {fixed = false; n = 0};
       backtrack board memory
   
    






  (* update : model -> model *)
let update ({ state; memory; board } as model) =
  match state with
  | Paused | Solved -> model
  | Running -> 
    match findFirstEmptyCell board with
    | None -> {model with state = Solved}
    | Some (row, col) -> 
      (match startingWith 1 board row col with
      | Some number -> 
        board.(row).(col) <- {n = number; fixed = false};
        Stack.push {row; col; number} memory;
        model
      | None ->
        (match backtrack board memory with
        | true -> model 
        | false -> failwith "No solution")
      )




(* finished : model -> bool *)
let finished model = model.state = Solved

(* handleMouse : model -> float -> float -> event -> model *)
let handleMouse model x y event =
  match event = "button_up" with
  | true  -> { model with state = toggle model.state }
  | false -> model

let go () =
  Animate.start initialModel
      ~name: "Sudoku"
      ~width: displayWidth
      ~height: displayHeight
      ~view: view
      ~rate: clockRate
      ~onTick: update
      ~onMouse: handleMouse
      ~stopWhen: finished
      ~viewLast: view

let s = go()
