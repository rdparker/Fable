module Fable.CLI.Printers

open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fable
open Fable.AST
open System.Collections.Generic

let findOverloads isInstance (e: FSharpEntity) =
    let add (m: FSharpMemberOrFunctionOrValue) (parentDic: Dictionary<_,_>) =
        let name = m.CompiledName
        if parentDic.ContainsKey(name) then
            // printfn "Overloads: %A" <| m.Overloads(false)
            let ar: ResizeArray<_> = parentDic.[name]
            ar.Add(m)
        else
            let ar = ResizeArray()
            ar.Add(m)
            parentDic.Add(name, ar)
    let overloads = Dictionary()
    for m in e.MembersFunctionsAndValues do
        if m.IsInstanceMember = isInstance then
            add m overloads
    overloads

let findOverloadIndex (m: FSharpMemberOrFunctionOrValue): int option =
    let findOverloadIndex' (m: FSharpMemberOrFunctionOrValue) (overloads: Dictionary<_,_>) =
        let name = m.CompiledName
        if overloads.ContainsKey(name) then
            let ar: ResizeArray<FSharpMemberOrFunctionOrValue> = overloads.[name]
            if ar.Count > 1 then
                if not m.IsInstanceMember then
                    printfn "Find overload %s in array with count %i" name ar.Count
                // .Equals() doesn't work. TODO: Compare arg types for trait calls
                // .IsEffectivelySameAs() doesn't work for constructors
                ar |> Seq.tryFindIndex(fun x ->
                    let res =
                        (m.CurriedParameterGroups, x.CurriedParameterGroups)
                        ||> (Seq.compareWith (Seq.compareWith (fun x y -> if x = y then 0 else -1)))
                    res = 0)
            else None
        else None
    m.EnclosingEntity |> Option.bind (fun e ->
        let overloads = findOverloads m.IsInstanceMember e
        findOverloadIndex' m overloads)

let attribsOfSymbol (s:FSharpSymbol) =
    [ match s with
        | :? FSharpField as v ->
            yield "field"
            if v.IsCompilerGenerated then yield "compgen"
            if v.IsDefaultValue then yield "default"
            if v.IsMutable then yield "mutable"
            if v.IsVolatile then yield "volatile"
            if v.IsStatic then yield "static"
            if v.IsLiteral then yield sprintf "%A" v.LiteralValue.Value

        | :? FSharpEntity as v ->
            v.TryFullName |> ignore // check there is no failure here
            match v.BaseType with
            | Some t when t.HasTypeDefinition && t.TypeDefinition.TryFullName.IsSome ->
                yield sprintf "inherits %s" t.TypeDefinition.FullName
            | _ -> ()
            if v.IsNamespace then yield "namespace"
            if v.IsFSharpModule then yield "module"
            if v.IsByRef then yield "byref"
            if v.IsClass then yield "class"
            if v.IsDelegate then yield "delegate"
            if v.IsEnum then yield "enum"
            if v.IsFSharpAbbreviation then yield "abbrev"
            if v.IsFSharpExceptionDeclaration then yield "exception"
            if v.IsFSharpRecord then yield "record"
            if v.IsFSharpUnion then yield "union"
            if v.IsInterface then yield "interface"
            if v.IsMeasure then yield "measure"
            // if v.IsProvided then yield "provided"
            // if v.IsStaticInstantiation then yield "static_inst"
            // if v.IsProvidedAndErased then yield "erased"
            // if v.IsProvidedAndGenerated then yield "generated"
            if v.IsUnresolved then yield "unresolved"
            if v.IsValueType then yield "valuetype"

        | :? FSharpMemberOrFunctionOrValue as v ->
            yield "owner: " + match v.EnclosingEntity with | Some e -> e.CompiledName | _ -> "<unknown>"
            // match v.Overloads(true) with
            // | Some overloads -> yield Seq.length overloads |> sprintf "overloads: %i"
            // | None -> ()
            match findOverloadIndex v with
            | Some i -> yield sprintf "overload idx: %i" i
            | None -> ()
            if v.IsActivePattern then yield "active_pattern"
            if v.IsDispatchSlot then yield "dispatch_slot"
            if v.IsModuleValueOrMember && not v.IsMember then yield "val"
            if v.IsMember then yield "member"
            if v.IsProperty then yield "property"
            if v.IsExtensionMember then yield "extension_member"
            if v.IsPropertyGetterMethod then yield "property_getter"
            if v.IsPropertySetterMethod then yield "property_setter"
            if v.IsEvent then yield "event"
            if v.EventForFSharpProperty.IsSome then yield "property_event"
            if v.IsEventAddMethod then yield "event_add"
            if v.IsEventRemoveMethod then yield "event_remove"
            if v.IsTypeFunction then yield "type_func"
            if v.IsCompilerGenerated then yield "compiler_gen"
            if v.IsImplicitConstructor then yield "implicit_ctor"
            if v.IsMutable then yield "mutable"
            if v.IsOverrideOrExplicitInterfaceImplementation then yield "override_impl"
            if not v.IsInstanceMember then yield "static"
            if v.IsInstanceMember && not v.IsInstanceMemberInCompiledCode && not v.IsExtensionMember then yield "funky"
            if v.IsExplicitInterfaceImplementation then yield "interface_impl"
            yield sprintf "%A" v.InlineAnnotation
            // if v.IsConstructorThisValue then yield "ctorthis"
            // if v.IsMemberThisValue then yield "this"
            // if v.LiteralValue.IsSome then yield "literal"
        | _ -> () ]

let rec printFSharpDecls prefix decls = seq {
    let mutable i = 0
    for decl in decls do
        i <- i + 1
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub) ->
            yield sprintf "%s%i) ENTITY: %s %A" prefix i e.CompiledName (attribsOfSymbol e)
            if not (Seq.isEmpty e.Attributes) then
                yield sprintf "%sattributes: %A" prefix (Seq.toList e.Attributes)
            if not (Seq.isEmpty e.DeclaredInterfaces) then
                yield sprintf "%sinterfaces: %A" prefix (Seq.toList e.DeclaredInterfaces)
            yield ""
            yield! printFSharpDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
            yield sprintf "%s%i) METHOD: %s %A" prefix i meth.CompiledName (attribsOfSymbol meth)
            yield sprintf "%stype: %A" prefix meth.FullType
            yield sprintf "%sargs: %A" prefix args
            // if not meth.IsCompilerGenerated then
            yield sprintf "%sbody: %A" prefix body
            yield ""
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            yield sprintf "%s%i) ACTION" prefix i
            yield sprintf "%s%A" prefix expr
            yield ""
}

let printFableDecls decls = seq {
    for decl in decls do
        yield sprintf "%A" decl
}

let printAst outDir (proj: FSharpCheckProjectResults) =
    if Directory.Exists(outDir) |> not then
        Directory.CreateDirectory(outDir) |> ignore
    for f in proj.AssemblyContents.ImplementationFiles do
        let target =
            let name = System.IO.Path.GetFileNameWithoutExtension(f.FileName)
            Path.Combine(outDir, name + ".fs.ast")
        Log.logVerbose(lazy sprintf "Print AST %s" target)
        printFSharpDecls "" f.Declarations
        |> fun lines -> System.IO.File.WriteAllLines(target, lines)
        // printFableDecls fableFile.Declarations
        // |> fun lines -> System.IO.File.WriteAllLines(Path.Combine(outDir, name + ".fable.ast"), lines)
