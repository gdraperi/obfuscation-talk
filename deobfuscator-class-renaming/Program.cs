using dnlib.DotNet;
using System.Globalization;

if (args.Length < 1)
{
    Console.WriteLine("deobfuscator-class-renaming mdfile targetfile");
    return;
}

var assemblyFile = args[0];
var targetFile = args[1];
ModuleContext modCtx = ModuleDef.CreateModuleContext();
ModuleDefMD module = ModuleDefMD.Load(assemblyFile, modCtx);
foreach (var type in module.Types)
{
    if (type.Name == "<Module>")
        continue;

    var renamingDictionary = new Dictionary<int, string>()
    {
        {0x2000004, "Program"},
    };
    Console.WriteLine($"Processing type: {type.FullName} (MDToken: {type.MDToken.ToInt32():X8})");
    if (renamingDictionary.TryGetValue(type.MDToken.ToInt32(), out var newName))
    {
        type.Name = newName;
        continue;
    }

    // Rename types based on hierarchy.
    if (type.BaseType.FullName == "System.Attribute")
        type.Name = type.Name + "Attribute";
    if (type.BaseType.FullName == "Avalonia.Controls.Window")
        type.Name = type.Name + "Window";
    if (type.BaseType.FullName == "Avalonia.Application")
        type.Name = type.Name + "App";
    if (type.BaseType.FullName == "Avalonia.Controls.UserControl")
        type.Name = type.Name + "UserControl";

    // Rename fields
    foreach (var field in type.Fields)
    {
        if (field.FieldType.FullName == "Avalonia.Controls.Button")
            field.Name = field.Name + "Button";
        if (field.FieldType.FullName == "Avalonia.Controls.Label")
            field.Name = field.Name + "Label";
    }
}

module.Write(targetFile);
Console.WriteLine($"Rewritten metadata for the assembly {assemblyFile} saved to {targetFile}");
