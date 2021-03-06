(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Recursive data types

  ------------------------------------
Name:            Ivan Ojiambo
TUT Student ID:   172663IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060 under your name, into a file
  coursework4/coursework4.fsx by October 20, 2017.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// In this coursework you will be required to design a file system data type
// that is able to carry the information about file names and access rights
// (read and/or write).
// The write permission on a directory is required to create or remove files
// in it, but not to write to an existing file in it.
// You will also be asked to create functions that can carry out various tasks
// on a file system.

// The access permissions are given by the following type:

type Permission = Read | Write | ReadWrite

// 0. Define a function
// createEmptyFilesystem: unit -> FileSystem
// that will be a function with 0 arguments that will return an
// empty filesystem of the type that you defined.
// (Permissions are initially assumed to be ReadWrite, check task 5)  
// We assume that your file system is defined in a type called FileSystem.

type FileSystem = (Permission*Element list)
and  Element =
    | File of FileInfo //(name, Permission)
    | Dir of DirectoryInfo
and FileInfo      = (string*Permission)
and DirectoryInfo = (string*FileSystem)


let createEmptyFilesystem() =
    (ReadWrite, List.empty<Element>)


//(ReadWrite, [File(name: "f1", permission: Read), Dir(name:"d2", (Write, []))])


// 1. Define two functions 
// createFile : string -> FileSystem -> FileSystem
// that will create a file into the root directory of the 
// current file system.
// The first argument is the file name, the second
// is the filesystem to create the file into. 
// (Permissions are initially assumed to be ReadWrite, check task 5)  

let  createFile (filename:string) (fs:FileSystem) = 
     let file = File (filename, ReadWrite)
     match fs with
     | (p,f) when p.Equals(ReadWrite) -> (p,[file]@f)
     | _ -> failwith "permission denied"
     
    

// createDir : string -> FileSystem -> FileSystem
// that will create a directory into the root directory of the current
// file system.
// The first argument is the name of the directory 
// (Permissions are initially assumed to be ReadWrite, check task 5)  

let createDir (dirname:string) (fs:FileSystem) = 
   let dir = Dir(dirname, fs)
   match fs with
   | (p, f)  when p.Equals(ReadWrite) || p.Equals(Write) -> (p, f@[dir])
   | _       -> failwith "permission denied"
  

// 2. Define a function 
// createSubDir : string -> FileSystem -> FileSystem -> FileSystem
// that will create a directory with name given in the first argument and
// contents given as the second argument into a file system given
// as the third argument.

let rec createSubDir (dirname:string)  (content:FileSystem) (filesys:FileSystem) = 
    let dir = Dir(dirname, content)
    let (p,fs) = filesys
    if not (p.Equals(Read)) then
        (p, fs@[dir])
    else
        failwith "permission denied"


  
     

// 3. Define a function
// count : FileSystem -> int
// that will recursively count the number of files in the current filesystem.

let count (filesystem:FileSystem) = 
    let rec loop filesys =
        match filesys with
        | (p, fs) -> matchList fs
    and matchList fs =
        match fs with 
        | Dir (_, fs1)::tail ->  (loop fs1) + matchList tail
        | File(_) :: tail       -> 1+ matchList tail
        | [] ->0
    loop filesystem


// 4. Define a function
// changePermissions Permission -> string list -> FileSystem -> FileSystem
// that will apply the specified permission the file or directory
// represented by a string list. For example, list ["Dir1";"Dir2";"File1"]
// represents a structure where "Dir1" is in the root directory, "Dir2" is
// in "Dir1" and "File1" is in "Dir2".

let  changePermissions(permission:Permission) (path:string list) (filesystem:FileSystem)  = 
 
    let rec loop  (path:string list) (elementlist) =
        match path  with
        | h::t -> matchFs h t elementlist
        | _  ->  elementlist
    and matchFs h t elementlist = 
        match elementlist with
        | Dir(name, (_, fs))::tl when name.Equals(h) && List.isEmpty t-> 
                Dir(name, (permission, fs))::tl

        | Dir(name, (p1, fs))::tl when name.Equals(h) && not (List.isEmpty t)-> 
                let fs1 = loop t fs 
                Dir(name, (p1, fs1))::tl

        | Dir(name, (p1, fs))::tl when not (name.Equals(h)) -> 
                let  fs2 = loop (h::t) tl 
                Dir(name, (p1, fs))::fs2
  
        | File(name, _)::tl  when  name.Equals(h) ->
                 File(name, permission)::tl

        | File(name, p1)::tl  when not (name.Equals(h))->
                let  fs2 =  loop (h::t) tl 
                File(name, p1)::fs2

        | _ -> failwith "Invalid path"
        
    let (p, elementlist) = filesystem
    if List.isEmpty path then
        (permission, elementlist)
    else
        let result = loop path elementlist
        (p, result)


// 5. Modify the implementations of createFile and createDir to honor the
// permissions of the current file system level, i.e. if the permission 
// of the current directory is not Write or ReadWrite, the function should fail
// with an exception and an appropriate message (that you should formulate yourself).
// Hint: use the built-in failwith function.    
     

// 6. Implement the function
// locate : string -> FileSystem -> string list list
// that will locate all files and directories with name matching the first argument
// of the function. The return value should be a list of paths to the files and
// directories. Each path is a list of strings indicating the parent directory
// structure.
// Note that the locate should honor the permissions, i.e. the files from
// directories without a read permission should not be returned.
let rec locate (name:string) (filesys: FileSystem) =
    let (_, elementlist) = filesys
    let rec loop elementlist (path:string list) (result)= 

        match elementlist with
        | File(n, p1)::tl ->
            if  p1.Equals(Write)  && name.Equals(n) || p1.Equals(ReadWrite) && name.Equals(n) then
                (path@[n]) :: loop tl (path) result
            else
                loop tl (path) result

        | Dir(n, (p1, elmlist))::tl ->
            if p1.Equals(Write) && name.Equals(n)  || p1.Equals(ReadWrite) && name.Equals(n) then
                (path@[n])::loop elmlist (path@[n]) (result) @ loop tl (path) (result)

            elif p1.Equals(Write) && not (name.Equals(n)) || p1.Equals(ReadWrite) && not (name.Equals(n)) then
                 loop elmlist (path@[n]) (result) @ loop tl (path) (result)
            else
                loop tl path result
        | [] -> result

    loop elementlist [] []

// 7. Implement the function
// delete : string list -> FileSystem -> FileSystem
// which will delete the file or directory specified with the first argument (the path
// represented by the string list) from a file system and return a file system
// not containing the file or directory represented by the first argument.
// If the file or directory does not have a write permission, the deletion should not
// be performed and the original file system should be returned.

let delete  (path: string list) (filesystem: FileSystem) =
    let rec loop path filesystem =
        match path with
        | h::t -> matchElmentList h t filesystem
        | _    -> filesystem
    
    and matchElmentList h t fs =
        match fs with 
        | File(n, p1)::tl  when not (p1.Equals(Read)) && n.Equals(h) -> tl

        | File(n, _) as f::tl  when  not (n.Equals(h)) ->
            let fs = loop (h::t) tl
            f::fs
        
        | Dir(n, (p1, _)):: tl when not (p1.Equals(Read)) && n.Equals(h) &&  List.isEmpty t -> tl

        | Dir(n, (p1, fs))::tl  when not (p1.Equals(Read)) && n.Equals(h) && not (List.isEmpty t) -> 
                let fs1 = loop t fs 
                Dir(n, (p1, fs1))::tl
           
        | Dir(n, _) as f1:: tl when  not (n.Equals(h)) ->
            let f = loop (h::t) tl
            f1::f

        | _ -> failwith " Invalid path" 
        
    let (p, fs)  = filesystem 
    let result = loop path fs
    (p, result)

// Bonus (1p):
// 8. Implement the function:
// recursiveDelete : string list -> FileSystem ->FileSystem
// that will delete a file or directory given as the first argument from a file
// system specified as the second argument.
// In case the item to be deleted is a directory, it needs to honor permissions
// and recursively only delete files with write permissions from directories with 
// write permissions. Subdirectories which will become empty need to be deleted as well. 

let recursiveDelete (path: string list) (filesystem: FileSystem) = 
    let rec loop path fs =
        match path with
        | h::t -> matchElmentList h t fs
        | _    -> fs
    
    and matchElmentList h t fs =
        match fs with 
        | File(n, p1)::tl  when not (p1.Equals(Read)) && n.Equals(h) -> tl

        | File(n, _) as f::tl  when  not (n.Equals(h)) ->
            let fs = loop (h::t) tl
            f::fs
        
        | Dir(n, (p1, fs)):: tl when not (p1.Equals(Read)) && n.Equals(h) &&  List.isEmpty t -> 
            let result = deleteContent fs
            Dir(n, (p1, result))::tl

        | Dir(n, (p1, fs))::tl  when not (p1.Equals(Read)) && n.Equals(h) && not (List.isEmpty t) -> 
                let fs1 = loop t fs 
                Dir(n, (p1, fs1))::tl
           
        | Dir(n, _) as f1:: tl when  not (n.Equals(h)) ->
            let  f = loop (h::t) tl
            f1::f

        | _ ->  fs 
    and deleteContent fs =
        match fs with
        | File(_, p2)::tl  when not (p2.Equals(Read)) ->  [] @ deleteContent tl

        | Dir(n, (p2, fs1))::tl  when not (p2.Equals(Read)) ->   
            let r1 = deleteContent fs1
            if List.isEmpty r1 then
                []
            else
                Dir(n, (p2, r1)) ::deleteContent tl
        | _ -> fs

    let (p, fs)  = filesystem 
    let result = loop path fs
    (p, result)

