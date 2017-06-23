// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO

type Label = Spam | Ham

type Corpus = {
    spamFreqs:Map<string, int>;
    hamFreqs:Map<string, int>;
    spamCount:int;
    hamCount:int;
    totalCount:int;
}

let classify (msg:string) : Label =
    Spam

let accuracyRate
    (classifier:string->Label)
    (labeledData:seq<Label*string>) : float =
        let filterFun = (fun data -> ((=) (fst data) (classifier (snd data))))
        let correctData = (Seq.filter filterFun labeledData)
        (float (Seq.length correctData)) / (float (Seq.length labeledData))

let string2label str =
    match str with
        | "ham" -> Ham
        | _ -> Spam

let makeRandomPredicate fractionTrue =
    let r = System.Random()
    let predicate x =
        r.NextDouble() < fractionTrue
    predicate

let features (msg:string) : seq<string> =
    // Most naive thing we could think of
    msg.Split( [|' '|] ) |> Array.toSeq

let countLabel (label:Label) (labeledWords : seq<Label*string>): int =
        labeledWords
        |> Seq.filter (fun (l,w)-> l = label)
        |> Seq.length

let frequencies label labeledWords : Map<string, int> =
        labeledWords
        |> Seq.filter (fun (l,w) -> l = label)
        |> Seq.groupBy (fun (l,w) -> w)
        |> Seq.map (fun (w,ws) -> (w, Seq.length ws))
        |> Map.ofSeq    

[<EntryPoint>]
let main argv = 
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "SMSSpamCollection.txt")
    let lines = System.IO.File.ReadAllLines(path)

    let labeledData =
        lines
            |> Seq.map (fun l -> l.Split( [|'\t'|] ))
            |> Seq.map (fun stra -> ((string2label stra.[0]), stra.[1]))

    let training, validation =
        labeledData
            |> Seq.toList
            |> List.partition (makeRandomPredicate 0.8)

    let words : seq<Label*seq<string>> =
        training
        |> Seq.map (fun (label, msg) -> (label, features msg))

    let labeledWords : seq<Label*string> =
        words
        |> Seq.collect (fun (l,words) -> Seq.map (fun w -> (l,w)) words)
    
    let corpus = {
        spamFreqs = (frequencies Spam labeledWords);
        hamFreqs = (frequencies Ham labeledWords);
        hamCount = (countLabel Ham labeledWords);
        spamCount = (countLabel Spam labeledWords);
        totalCount = (Seq.length labeledWords);
    }

    printfn "The accuracy is %A" (accuracyRate classify validation)
    printfn "Training size: %A" training.Length
    printfn "Total=%A, Ham=%A, Spam=%A" corpus.totalCount corpus.hamCount corpus.spamCount
    printfn "'the' occurs %A in Ham" (corpus.hamFreqs.Item "the")
    0 // return an integer exit code
