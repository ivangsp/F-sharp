
(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher order functions

  ------------------------------------
  Name: IVAN OJIAMBO
  TUT Student ID: 172663IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060 under your name, into a file coursework3/coursework3.fsx by October 11, 2017.
  NB! The deadline has been extended!
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem data structure based on discriminated unions such that it supports data
// for bibliography items for
// * article (journal paper)
// * inproceedings (conference paper)
// * book
// * MSc thesis
// * Web page (misc)
// Use the specifications given at http://newton.ex.ac.uk/tex/pack/bibtex/btxdoc/node6.html
// You should support all mandatory and optional fields of each entry. The names of the fields should
// be the same as in the referenced web page. You should capitalize the names of constructors in 
// discriminated unions.



type Article = {
      author:string list
      title:string
      journal:string
      year:int
      
      pages: (int*int) option
      volume:int        option
      number: int       option
      month: int       option
      note:string      option
 }
type Inproceeding = {
      author:string list
      title:string
      booktitle:string
      year:int
       
      pages:(int*int)       option
      address:string        option
      month:int             option
      editor:string         option
      volume:int            option
      series:string         option
      number: int           option
      organization: string  option
      publisher: string     option
      note: string          option
        }

type Book = {
     author:string list option
     editor:string list option
     title:string
     publisher:string
     year:int

     volume:int         option
     number:int         option
     series:int      option
     address:string     option 
     edition: string    option
     month:int       option
     note:string        option
    }

type MastersthesisRecord = {
     author:string list
     title:string
     school:string
     year:string

     address:string   option
     month:int        option
     note:string      option
     type_:string      option

 }
 
 type MiscRecord = {
      author:string  list    option
      title:string           option
      howpublished:string    option
      month:int              option
      year:int               option
      note:string            option
 }

let optionList(value:string list option) =
    match value with
    |None ->[]
    |Some a ->a

let optionIntValue(value: int option) =
    match value with
    |None ->0
    |Some a ->a

let optionStrinValue(value: string option) =
    match value with
    |None ->""
    |Some a ->a


type  BibliographyItem = 
    |Articles of Article
    |Inproceedings of Inproceeding
    |Books of Book
    |Mastersthesis of MastersthesisRecord
    |Misc of MiscRecord


// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 10 different publications on your favourite topic from http://dblp.uni-trier.de/ 
// and the MSc thesis databases of UT and TTÜ. At least one 
//instance of every publication needs to be 
// Please note that you need not read the papers, just pick 10 papers that 
//sound interesting to you from the database.

let bibliographyData = 
    [
    Articles({author=["Julyan Arbel"; "Igor Prünster"]; 
                title="A moment-matching Ferguson & Klass algorithm"; journal="Statistics and Computing";
                year=2017; volume=None; number=None; pages=Some(3,17); month=None; note=None});

    Articles({author=["Eugen Pircalabelu"; "Gerda Claeskens" ;"Irène Gijbels"]; 
            title="Copula directed acyclic graphs";
            journal = "Statistics and Computing";  year=2017; pages=Some(55, 78); 
            number=None;  month=None; note=None;  volume=None});

    Articles({author=["Gabriele Mayr"; "Günter Lepperdinger"; "Peter Lackner"]; 
              title="Automated Quantitative Assessment of Proteins";
              journal="Advances in Bioinformatics"; year=2008; pages=Some(1, 7);  
              number=None;  month=None; note=None;   volume=None});

    Inproceedings({author=["Michael S. Brown"]; number=None; publisher=None;
                 title="2015 International Conference on 3D Vision";note=None;
                 volume=None; series=None; booktitle="x-Hour Outdoor Photometric Stereo"; 
                 year=2015; pages=Some(19, 22); 
                 address=Some"Lyon, France"; month=Some 10; editor=None; organization=None});

    Inproceedings({author= ["Rosa Bottino"]; number=None; publisher=None;
                 title="Games and Learning Alliance - GALA"; note=None;
                booktitle="5th International Conference"; volume=None; 
                series=None;  year=2016; pages=Some(5,7); address=Some"Netherlands"; 
                month=Some 12; editor=None; organization=None});
    
    Books({author=Some["M. Guadalupe"; "Sánchez-Escribano"]; title="Engineering Computational Emotion"; 
            publisher="ISBN 978-3-319-59429-3"; year=2018; volume=None;series=None; address=None;
            edition=None; month=None;note=None; number=None; editor=None});

    Books({author=Some["Mark Hoogendoorn,Burkhardt Funk"]; 
            title="Machine Learning for the Quantified Self - On the Art of Learning from Sensory Data"; 
            publisher="ISBN 978-3-319-66307-4"; year=2017; volume=None;series=None; address=None;
            edition=None; month=None;note=None;  number=None; editor=None});

    Mastersthesis({author=["Afu, Immaculate Ache"]; 
              title="Migration and brain drain effects in Cameroon. ";
              school="Tallinn University of Technology ";
              year="2016"; month=Some 1; address=None; type_=None; note=None });

    Mastersthesis({author=["Aksjonov, Andrei "]; 
            title=" 3D crane control system. The Modeling and Control of 3D Crane ";
            school="Tallinn University of Technology "; year="2015"; month=Some 6; address=None;
            type_=None; note=None });

    Misc({author=None; title=Some"TTCN-3 test development and execution platform"; 
            year=Some 2012; month= Some 10;
            howpublished=Some"http://www.elvior.com/testcast/introduction"; note=None})
   
    ]

// 3. Create a function formatInACMReferenceStyle : BibliographyItem ->
// string that will format the bibliography items
// using the reference style specified here: http://www.acm.org/publications/authors/reference-formatting
let formatAuthor(authors: string list) =         
    let rec loop (authors: list<string>) (acc:string) =
        match authors with
        |[hd] -> acc+hd+"."
        |hd::tl    when (tl.Length =1 )  -> loop (tl) (acc+hd+" and ") 
        |hd::tl -> loop (tl) (acc+hd+", ")
        |[] ->acc
    loop (authors) ""

let monthToStr mon =
    match mon with 
    |1->"jan" 
    |4->"Apr" 
    |7->"Jul"
    |10->"Oct"
    |2->"Feb" 
    |5->"May" 
    |8->"Aug" 
    |11->"Nov"
    |3->"Mar" 
    |6->"Apr" 
    |9->"Sep"
    |12->"Dec"
    |_ ->""
    

let pagesToStr (pages:(int*int)) = 
    match pages with 
    |(a, b) ->a.ToString()+"-"+b.ToString()+"."
    |_ -> ""

let optionToStr value =
    match value with 
    | Some x -> x
    | None   -> ""

let optionToList value =
    match value with 
    | Some x -> x
    | None   -> []

let optionToInt value = 
    match value with 
    | Some x -> x
    | None   -> 0

let optionToTuple value = 
    match value with 
    | Some (a,b) -> a.ToString()+"-"+b.ToString()
    | None -> ""


let formatInACMReferenceStyle(item:BibliographyItem) = 
    match item with
    |Articles a           -> formatAuthor(a.author) + "." + a.year.ToString() + "." + " " + a.title + " "
                                    + a.journal + "," + " " + optionToInt(a.number).ToString()
                                 + " (" + monthToStr(optionToInt(a.month)) + "." + a.year.ToString() 
                                 + "),"+ optionToTuple(a.pages)
    |Inproceedings  i     -> i.author.ToString() + "." + i.year.ToString() + "." + " " + i.title+". " + 
                             i.booktitle + "(" + optionToStr(i.series)+")" + "."

    |Books b              -> formatAuthor (optionToList(b.author)) + "."+b.year.ToString()+"." + " "+ b.title+". "+
                             optionToInt(b.series).ToString() + "."

    |Mastersthesis m      ->formatAuthor(m.author)  + "." + m.year.ToString() + ". " + m.title + ", " + "." + 
                             optionToStr(m.address) 
    |Misc m               -> formatAuthor(optionToList(m.author)) + "."+optionToInt(m.year).ToString()+". "+
                             "(" + monthToStr(optionToInt(m.month))+" "+ optionToInt(m.year).ToString() + ")."


// 4. Write a function compareByAuthorYear : BibliographyItem -> BibliographyItem -> int that 
//will compare the authors and year of the
// bibliography item in the same way as specified in coursework2.


let getAuthorAndYear (b_item1: BibliographyItem) =
    match b_item1 with
    |Articles a           ->(a.author, a.year)
    |Inproceedings  i     ->(i.author, i.year)
    |Books b              ->(optionToList b.author, b.year)
    |Mastersthesis m      ->(m.author, int m.year)
    |Misc m               ->(optionToList(m.author), optionIntValue m.year)
 
let compareByAuthorYear (b_item1: BibliographyItem)  (b_item2: BibliographyItem) =  
    let (authorlist2, year2) = getAuthorAndYear(b_item2)
    let (authorlist1, year1) = getAuthorAndYear (b_item1)
    match compare authorlist1 authorlist2 with
    | 0->compare year1 year2
    | i -> if i < 0 then -1 else 1

// 5. Write a function orderBibliography: (BibliographyItem -> BibliographyItem -> int) ->
// BibliographyItem list -> BibliographyItem list
// That will order the list of bibliography items according to the given comparison function.

let orderBibliograph (orderby: (BibliographyItem -> BibliographyItem -> int)) (item: BibliographyItem list) =
    item |>List.sortWith (fun x y -> orderby x y)

// 6. Write a function formatBibliographyItems : (BibliographyItem -> string)
// -> BibliographyItem list -> string list that will take
// a formatting function and a bibliography list and produce a string list that contains formatted bibliography.

let formatBibliographyItems  (formatItem: BibliographyItem->string) (item: BibliographyItem list) =
    item
        |>List.map(formatItem )  



// 7. Write a function getNumberedBibliography : BibliographyItem list -> string
// that contains a numbered bibliography where each bibliography item is preceded with 
//a sequence number surrounded
// by square brackets [] and ends with a newline character '\n'.
// The implementation should involve List.fold or List.foldBack function, whichever you deem appropriate.

let getindex list ele =
    let index = list |> List.findIndex(fun x -> x = ele)
    index + 1

let getNumberedBibliography (item:BibliographyItem list) =
    item
    |> List.map(fun x ->"[" + (getindex item x).ToString() + "]" + formatInACMReferenceStyle(x))
    |> List.fold(fun x s-> x + s + "\n")" "




// 8. Create 5 appropriate functions to create BibliographyItem data instances. Please note that 
// it is up to you to define the internal data structure. The following functions will be used 
//for generating data in your
// format.
(* 
createArticle :
  author:string list ->
    title:string ->
      journal:string ->
        year:int ->
          volume:int option ->
            number:int option ->
              int * int option ->
                month:int option ->
                  note:string option -> BibliographyItem

*)
let createArticle (author:string list) (title:string) (journal:string) (year:int)
  (volume:int option) (number:int option) (page: (int * int) option) (month:int option) ( note:string option)=
    Articles {author=author; title=title; journal=journal; year=year; volume=volume;number=number;
              pages=page; month=month; note= note}

(*
createBook :
  author:string option ->
    editor:string option ->
      title:string ->
        publisher:string ->
          year:int ->
            volume:int option ->
              number:int option ->
                series:int option ->
                  address:string option ->
                    edition:string option ->
                      month:int option ->
                        note:string option -> BibliographyItem
*)
let createBook( author:string list option) (editor:string list option) (title:string) (publisher:string) (year:int)
  (volume:int option) (number:int option) (series:int option) (address:string option) ( edition:string option)
  (month:int option) (note:string option) =
    Books {
      author=author; edition=edition; editor=editor; title=title; publisher=publisher; year=year;
      volume=volume; number=number; series=series; address=address; month=month; note=note;
      }
(*
createInProceedings :
  author:string ->
    title:string ->
      booktitle:string ->
        year:int ->
          editor:string option ->
            volume:int option ->
              number:int option ->
                series:string option ->
                  int * int option ->
                    address:string option ->
                      month:int option ->
                        organization:string option ->
                          publisher:string option ->
                            note:string option -> BibliographyItem
*)

let createInProceedings (author:string list)(title:string) ( booktitle:string) (year) (editor:string option)
  (volume:int option) (number:int option) (series:string option) (pages:(int * int) option) 
  (address:string option) (month:int option) (organization:string option) (publisher:string option)
  (note:string option) =
    Inproceedings {
        author=author; title=title; booktitle=booktitle; year=year; editor=editor;
        volume=volume; number=number; series=series; pages=pages;  address=address;
        month=month; organization=organization; publisher=publisher; note=note
        }

(*
createMScThesis :
  author:string ->
    title:string ->
      school:string ->
        year:string ->
          type_:string option ->
            address:string option ->
              month:int option -> note:string option-> BibliographyItem

*)
let createMScThesis(author:string list) (title:string) (school:string) (year:string) (type_:string option)
    (address:string option) (month:int option) (note: string option) = 
    Mastersthesis {
        author=author; title=title; school=school; year=year; type_=type_; address=address; month=month; note=note
    }

(*
createMisc :
  author:string option ->
    title:string option ->
      howpublished:string option ->
        month:int option ->
          year:int option ->
            note:string option -> BibliographyItem

*)

let createMisc (author:string list option) (title:string option) (howpublished:string option) (month:int option)
    (year:int option) (note:string option) =
    Misc {
        author = author; title=title; howpublished=howpublished; month=month; year=year;note=note;
    }
