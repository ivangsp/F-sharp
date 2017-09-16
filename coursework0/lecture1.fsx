(*

  ITT8060 -- Advanced Programming 2017
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 0: Getting started

  ------------------------------------
  Name: IVAN OJIAMBO
  Student ID:
  ------------------------------------

  Answer the questions below.  You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks.

*)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
//    I.e. log in to a lab machine and start Visual Studio, install
//    VS/Mono on your laptop, etc.

// 1. Load the following function into fsi

let greeting name = printfn "Hello: %s" name 

// 2. Run the function 'greeting' and say hello to yourself.
//    Type the expression below.
greeting "Ivan Ojiambo"

// 3. Define
//    'splitAtChar : text:string -> sep:char -> list<string>'
let splitAtChar (text:string) (sep:char) =
    text.Split[|sep|] |>Array.toList
    
// 4. Modify splitAtSpaces to use splitAtChar
let splitAtSpaces (text:string)  =
    splitAtChar text ' '
   
   
// 5. Define 'sentenceCount : text:string -> int'
let sentenceCount (text:string) =
   splitAtChar text '.' |> List.length


// 6. Define 'stats : text:string -> unit'
//    which prints the same stats as showWordCount +
//    the number of sentences and average length of sentences
//    hint: try float: 'int -> float'

let stats(text:string) =
   let numSentences =text |> sentenceCount
   let avgLength = 
        splitAtChar text '.' 
        |>List.map(fun x-> x|>String.length)
        |>List.averageBy(fun x -> float x)
 
   printfn" The number of sentences %d" numSentences
   printfn "Average length of sentence  %f" avgLength  

// 7. Use the 'http' function from the lecture to download the file
//    http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
//    NOTE: you cannot use this function in tryfsharp. Instead you can
//    paste the text into your file as a string and process it locally
open System.IO
open System.Net
let http (url:string) = 
    let request     = System.Net.WebRequest.Create(url)
    let response    = request.GetResponse()
    let stream      = response.GetResponseStream()
    let reader      = new StreamReader(stream)
    let html        = reader.ReadToEnd()
    response.Close()
    html

// 8. run 'stats' on the downloaded file
stats(http("http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt"))
