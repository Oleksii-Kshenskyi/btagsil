using btagsil;

World world = REPL.CreateWorld();
while (true) {
    Console.Write(Data.System.Prompt);
    var theAction = REPL.ParseAction(ReadSingleLine());
    Console.Write($"{theAction.Execute(world)}");
    if(theAction is not Empty) Console.Write("\n\n");
}

static string[] ReadSingleLine() {
    var line = Console.ReadLine()?.Trim().ToLower().Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
    return line?.Length > 0 ? line : new string[] { "" };
}
