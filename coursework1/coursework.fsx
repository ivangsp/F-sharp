(*
    Department of Software Science
    Tallinn University of Technology
    ------------------------------------

    Coursework 1: Basic operations on lists

    ------------------------------------
    Name:             Ivan Ojiambo
    TUT Student ID:   172663IVSM
    ------------------------------------


    Answer all the questions below.  You answers to questions should be
    correct F# code written after the question in comments. This file is an F#
    script file, it should be possible to load the whole file at
    once. If you can't then you have introduced a syntax error
    somewhere and your result will not be evaluated.

    This coursework will be graded.

    To submit the coursework you will be asked to

    1) Check out your  GIT repository
    from the server gitlab.cs.ttu.ee using instructions on page
    https://courses.cs.ttu.ee/pages/ITT8060

    2) Put your solution into a file coursework1/coursework1.fsx
    in the repository. Commit it and push it to the server!
    It is your responsibility to make sure you have pushed the solution
    to the repository!

    NB! It is very important to make sure you use the exact name using
    only small caps. Files submitted under wrong name may not get a grade.

    Also, use the exact function and identifier names with precise types as 
    specified in the question.

 
*)

// 1. Associate an identifier "myFirstList" with an empty list of type 'float list' (or list<float>).
let myFirstList:List<float> = [] 

// 2. Write a function
// count 'a list -> int
// that will return a number of elements in a list. 
let count list =List.length(list)

// 3. Make a list of cantines available on TTU campus containing 4-tuples (quadruples).
// The identifier of the list should be "cantines". The 4-tuples should be of type string * string * int * int
// The elements of the 4-tuples should represent the following:
//   1) The name of the cantine
//   2) The building identifier
//   3) The closing hour as integer
//   4) The closing minutes as ingeger
let cantines:(string*string*int*int)list =[
  
    ("Economics- and social science building canteen","SOC- building", 18, 30);
    ("Libary canteen","Akadeemia tee 1", 19, 00);
    ("Main building Deli cafe", "U01 building",16, 30);
    ("Natural Science building cantee", "SCI building", 16, 00)
]
 
// 4. Write a function currentlyOpen: int -> int -> (string * string * int * int) list -> string list
// that will return the building identifiers where cantines have not yet closed. The first argument is the
// current hours as integer, the second is the current minutes as integer, the third is the list of cantines 
// with the same type as you specified in previous question.
// Your solution should do the filtering explicitly.
  
let  isOpen( current_hour:int, closing_min:int,  cateens:(string*string*int*int)) = 

    match cateens with
    |(_, x, y, z) when current_hour < y -> x 
    |(_, x, y, z) when current_hour = y &&  closing_min < z -> x
    |(_, x, y, z)  ->""
        
let currentlyOpen(current_hour:int,  current_minute:int,  cateen:(string*string*int*int)list) =         
    let rec loop (cateens, acc) =
        match cateens with
        |head::tail when (isOpen(current_hour, current_minute, head).Length >0)  ->loop (tail, (isOpen(current_hour, current_minute, head)::acc))
        |head::tail when (isOpen(current_hour, current_minute, head).Length <0) -> loop(tail, acc)
        |[]->acc
        |_-> acc
       
        

    loop(cateen, ["hello"])

currentlyOpen (18, 34, cantines)

// 5. Write a function currentlyOpen2: int -> int -> (string * string * int * int) list -> string list
// that behaves similarily to the currentlyOpen, but uses List.filter in its implementation.
      
let isOpen2 (current_hour:int, current_minute:int, cateen:(string*string*int*int)) = 
        match cateen with
        |(_, x, y, z) when current_hour < y -> true
        |(_, x, y, z) when current_hour = y &&  current_minute < z -> true
        |(_, x, y, z)  ->false


let currentlyOpen2  (current_hour:int, current_minute:int, cateen:(string*string*int*int)list) = 
    cateen 
        |> List.filter(fun x ->isOpen2 (current_hour, current_minute, x))
        |>List.map(fun (r, x, y, z) ->x)

currentlyOpen2 (18, 50, cantines)

       

   