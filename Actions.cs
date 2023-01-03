namespace btagsil
{
    public interface IAction
    {
        string Execute(World world);
    }

    public class Exit: IAction {
        public Exit() { }
        public string Execute(World world) {
            Console.Write($"{Data.Exit.Message}\n\n");
            Environment.Exit(0); return "";
        }
    }

    public class Empty: IAction {
        public Empty() { }
        public string Execute(World world) => "";
    }

    public class Unknown : IAction {
        public Unknown(string verb) => _verb = verb;
        public string Execute(World world) => Data.Unknown.With(_verb);
        private readonly string _verb;
    }
    public class Echo : IAction {
        public Echo(string[]? echoed) => _echoed = echoed;
        public string Execute(World world) => _echoed == null ? Data.Echo.What : Data.Echo.With(string.Join(" ", _echoed));
        private readonly string[]? _echoed;
    }
    public class WhereWhat : IAction {
        public string Execute(World world) => Data.Where.What;
    }
    public class WhereAmI : IAction {
        public string Execute(World world) {
            var curLoc = world.CurrentLocation;
            return $"You're in the {curLoc.Name}. It's {curLoc.Description}.";
        }
    }
    public class UnknownWhere : IAction {
        public string Execute(World world) => Data.Where.Unknown;
    }
}
