namespace btagsil
{
    public static class REPL
    {
        public static World CreateWorld() => new();
        public static Exit ParseExit() => new();
        public static Empty ParseEmpty() => new();
        public static Unknown ParseUnknown(string[] words) => new(words[0]);
        public static Echo ParseEcho(string[] words) => words.Length switch {
            1 => new(null),
            > 1 => new(words.Skip(1).ToArray()),
            _ => throw new ArgumentException(Data.Echo.ZeroWordsError)
        };
        public static IAction ParseAction(string[] words) => words[0] switch
        {
            "exit" => ParseExit(),
            "echo" => ParseEcho(words),
            "" => ParseEmpty(),
            _ => ParseUnknown(words)
        };
    }
}
