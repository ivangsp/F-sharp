 (*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name:             Ivan Ojiambo
    TUT Student ID:   172663IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060 under your name, into a file coursework2/coursework2.fsx by September 29, 2017.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem that has the following structure:
// string list * string * int * (int * int)
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents the year of publication
// * The fourth field represents a pair containing the starting page number and ending page number of the paper.

type BibliographyItem = string list * string * int * (int * int)


// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 10 different publications on your favourite topic from http://dblp.uni-trier.de/ 
// Please note that you need not read the papers, just pick 10 papers that sound interesting to you from the database.

let bibliographyData:(List<BibliographyItem>) = [

    (["Sergio, Miranda"; "Francesco, Orciuoli"; "Vincenzo, Loia"; "Demetrios, G. Sampson:"], "An ontology-based model for competence management", 2017, (51,66));
    (["Xiao-Feng, Wang"; "Zhaozhi, Fan"; "Bin, Wang"], "Estimating smooth distribution function in the presence of heteroscedastic measurement error", 2010, (25, 36));
    (["Renato, Coppi"; "María, Angeles Gil"; "Henk, A. L. Kiers"], "The fuzzy approach to statistical analysis", 2005, (1, 14));
    (["Huiwen, Wang"; "Qiang, Liu"; "Yongping, Tu"], "nterpretation of partial least-squares regression models with VARIMAX rotation", 2005, (207, 219))

     
]

// 3. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns 
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You are encouraged to use String.Compare to compare individual strings. If the first authors are the same
// then the precedence should be determined by the next author.
// A missing author can be considered to be equivalent to an empty string.
// Please note that your implementation should be recursive over the input lists.

let rec compareLists (list1:string list, list2:string list) = 
    let list1:string list   = List.sort(list1)
    let list2:string list   = List.sort(list2)
    match list1, list2 with
    |(head1::tail1), (head2::tail2)  ->head1.CompareTo head2 
    |(head1::tail1), (head2::tail2)  when head1.CompareTo head2 =0  -> compareLists (tail1 ,tail2)
    |[], _ -> -1
    |[],[] -> 0
    |_,[]  -> 1

// 4. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
let compareAuthors (b_item1:BibliographyItem, b_item2:BibliographyItem) = 
    match b_item1, b_item2 with
    |(list1, _, _, _), (list2, _, _, _) -> compareLists (list1, list2)
   

// 5. Make a function
// compareAuthorsYears : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are 
// the same then according to years.


let compareYears(year1:int, year2:int) = 
    match year1, year2 with
    | year1, year2 when(year1 > year2) -> 1
    | year1, year2 when(year1 < year2) -> -1
    |_ ->0

let compareAuthorsYear (b_item1:BibliographyItem, b_item2:BibliographyItem) = 
    match b_item1, b_item2 with 
    |(list1, _, year1,_), (list2, _, year2, _)  when (compareAuthors (b_item1, b_item2)=0) ->compareYears(year1, year2)
    |(list1, _, year1,_), (list2, _, year2, _) -> compareAuthors (b_item1, b_item2)
    

// 6. Make a function 
// sortBibliographyByYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the year in ascending order

let sortBibliographyByYear (b_list1:BibliographyItem list) : BibliographyItem list= 
    b_list1 |> List.sortBy(fun(_, _, year, _) -> year)  


// 7. Make a function 
// sortBibliographyByAuthorYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and year in ascending order



// 8. Make a function
// groupByYear : BibliographyItem list -> BibliographyItem list list
// where the return list contains lists of bibliography items published in the same year.

let groupByYear (b_list: BibliographyItem list)  = 
    b_list 
        |>  Seq.groupBy(fun (_, _, year, _)  -> year)
        |> List.ofSeq
        |> List.map( fun (x, y) -> y|> List.ofSeq)



// 9. Make a function
// commaSeparatedList : BibliographyItem list -> string
// That will return a comma separated string representation of the data.
// Use function composition operator "<<" in your implementation.

// let escapeChar (stringvalues:string list) =
//     let rec loop (stringvalues:string list, acc) =
//         match stringvalues with
//         |hd::tail when hd.Contains(",") -> loop(tail, (System.String.Format("\"{0}\"", hd) + ","+acc)) 
//         |hd::tail  -> loop(tail, hd+","+acc)
//         |[]-> acc
//         |_ -> acc
//     loop (stringvalues, "")

let escapeChar (stringvalues:string list) =
    let rec loop (stringlist:string list, acc) =
        match stringlist with
        |hd::tail when hd.Contains(",") -> loop(tail, (System.String.Format("\"{0}\"", hd) + ","+acc)) 
        |hd::tail  -> loop(tail, hd+","+acc)
        |[]-> acc
    loop (stringvalues, "")


let commaSeparatedLis (b_itemlist: BibliographyItem list) =
    b_itemlist 
        |>List.map(fun (a, t, y, (p,q)) -> escapeChar(a) + "," + t + ","+ y.ToString() + ","+ p.ToString()+"-"+q.ToString())
        |> List.fold(fun s f -> s + f + "\n") ""
                   
commaSeparatedLis bibliographyData 

//hint::using UTF-8

// |> List.map(fun (a,t, y, p) ->(a|> List.fold (fun acc x -> (acc+",") + x ) " "), t, y.ToString() )