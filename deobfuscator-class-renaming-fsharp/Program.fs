open System
open dnlib.DotNet
open System.Collections.Generic

[<AutoOpen>]
module FsExtensions =

    type IDictionary<'Key,'Value> with
        /// Attempts to get the value associated with the specified key.
        member this.TryGet key =
            let ok, v = this.TryGetValue key
            if ok then Some v else None

let renameField(field: FieldDef) =
    if (field.FieldType.FullName = "Avalonia.Controls.Button") then
        Some (UTF8String.ToSystemString field.Name  + "Button")
    elif (field.FieldType.FullName = "Avalonia.Controls.Label") then
        Some (UTF8String.ToSystemString field.Name + "Label")
    else
        None

let renameType (type_: TypeDef) =    
    // Rename types based on hierarchy.
    if (type_.BaseType.FullName = "System.Attribute") then
        Some (UTF8String.ToSystemString type_.Name + "Attribute")
    elif (type_.BaseType.FullName = "Avalonia.Controls.Window") then
        Some (UTF8String.ToSystemString type_.Name + "Window")
    elif (type_.BaseType.FullName = "Avalonia.Application") then
        Some (UTF8String.ToSystemString type_.Name + "App")
    elif (type_.BaseType.FullName = "Avalonia.Controls.UserControl") then
        Some (UTF8String.ToSystemString type_.Name + "UserControl")
    else
        None

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        Console.WriteLine("deobfuscator-class-renaming mdfile targetfile")
        1
    else
        let assemblyFile = args[0]
        let targetFile = args[1]
        let modCtx = ModuleDef.CreateModuleContext()
        let module_ = ModuleDefMD.Load(assemblyFile, modCtx)
        for type_ in module_.Types do
            if UTF8String.ToSystemString type_.Name <> "<Module>" then
                let renamingDictionary = dict [
                    (0x2000004, "Program");
                ]
                Console.WriteLine($"Processing type: {type_.FullName} (MDToken: {type_.MDToken.ToInt32():X8})");
                match renamingDictionary.TryGet (type_.MDToken.ToInt32()) with
                | Some newName ->
                    type_.Name <- newName;
                | None -> 
                    match renameType(type_) with
                    | Some newName -> type_.Name <- newName
                    | None -> ()

                // Rename fields
                type_.Fields |> Seq.iter(fun field ->
                    match renameField(field) with
                    | Some newName -> field.Name <- newName
                    | None -> ()
                )

        module_.Write(targetFile);
        Console.WriteLine($"Rewritten metadata for the assembly {assemblyFile} saved to {targetFile}");
        0