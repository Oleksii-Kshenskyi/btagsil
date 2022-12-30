using btagsil;

World world = REPL.CreateWorld();
while (true) {
    Console.Write(Data.System.Prompt);
    REPL.ParseAction(ReadSingleLine()).Execute(world);
}

static string[] ReadSingleLine() {
    var line = Console.ReadLine()?.Trim().ToLower().Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
    return line?.Length > 0 ? line : new string[] { "" };
}
