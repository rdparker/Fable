module QuickTest

// Use this template to make quick tests when adding new features to Fable.
// You must run a full build at least once (from repo root directory,
// type `sh build.sh` on OSX/Linux or just `build` on Windows). Then:
// - When making changes to Fable.Compiler run `build QuickFableCompilerTest`
// - When making changes to fable-core run `build QuickFableCoreTest`

// Please don't add this file to your commits

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual
    if not areEqual then
        failwithf "Expected %A but got %A" expected actual

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// [<Test>]
// let ``My Test``() =
//     Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
//     Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4

// You'll have to run your test manually, sorry!
// ``My Test``()

type Foo() =
    new(_i: int) = Foo()
    member __.Subtract(x,y) = x - y
    member __.Add(x) = x + 5
    member __.Add(x, y) = x + y
    [<CompiledName("Foo")>]
    static member Add(x, y) = x + y
    static member Add(x, y, z) = x + y + z
    static member Add(x, y, z, u) = x + y + z + u
    static member (+) (x, y) = x + y
    static member (+) (x, y) = x + Int32.Parse(y)
    // static member (+) (x, y, z) = x + y + z
