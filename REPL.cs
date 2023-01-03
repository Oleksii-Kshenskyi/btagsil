namespace btagsil
{
    public static class REPL
    {
        public static World CreateWorld() => new();
        public static IAction ParseAction(string[] words) => words[0] switch
        {
            "exit" => ParseExit(),
            "echo" => ParseEcho(words),
            "where" => ParseWhere(words),
            "go" => ParseGo(words),
            "look" => ParseLook(words),
            "" => ParseEmpty(),
            _ => ParseUnknown(words)
        };

        private static Exit ParseExit() => new();
        private static Empty ParseEmpty() => new();
        private static Unknown ParseUnknown(string[] words) => new(words[0]);
        private static Echo ParseEcho(string[] words) => words.Length switch
        {
            1 => new(null),
            > 1 => new(words.Skip(1).ToArray()),
            _ => throw new ArgumentException(Data.Echo.ZeroWordsError)
        };
        private static IAction ParseWhere(string[] words) => words switch
        {
            ["where"] => new WhereWhat(),
            ["where", "am", "i"] => new WhereAmI(),
            ["where", "can", "i", "go"] => new WhereCanIGo(),
            _ => new UnknownWhere(),
        };
        private static IAction ParseGo(string[] words) => words switch
        {
            ["go"] => new GoForgotTo(),
            ["go", "to"] => new GoToWhere(),
            ["go", "to",.. var location] => new GoTo(string.Join(" ", location)),
            _ => new UnknownGo(),
        };
        private static IAction ParseLook(string[] words) => words switch
        {
            ["look"] => new LookOnlyAtStuff(),
            ["look", "at"] => new LookAtWhat(),
            ["look", "at", .. var obj] => new LookAt(string.Join(" ", obj)),
            ["look", "around"] => new LookAround(),
            _ => new UnknownLook()
        };
    }
}
