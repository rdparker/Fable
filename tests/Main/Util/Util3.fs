// Test that a file with multiple namespaces works, see #1218

// Different namespace together with namespaces
// sharing a prefix works
namespace My.Namespace
type Helper =
    static member Add1(x) = x + 1

// Duplicating namespaces works
namespace Fable.Tests.A.C
type Helper2 =
    static member Add2(x) = x + 2

// Empty namespaces work
namespace Fable.Tests.A.D

// Multiple mamespaces sharing prefix work
namespace Fable.Tests.A.B
type Helper =
    static member Add3(x) = x + 3

namespace Fable.Tests.A.C
type Helper =
    static member Add5(x) =
        My.Namespace.Helper.Add1(x - 1)
        |> Helper2.Add2
        |> Fable.Tests.A.B.Helper.Add3

// module Test =
//     Helper.Add5(10) |> printfn "RESULT:%i"