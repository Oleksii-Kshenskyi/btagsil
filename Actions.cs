namespace btagsil
{
    public interface IAction
    {
        void Execute(World world);
    }

    public class Exit: IAction {
        public Exit() { }
        public void Execute(World world) {
            Console.WriteLine(Data.Exit.Message);
            Environment.Exit(0);
        }
    }

    public class Empty: IAction {
        public Empty() { }
        public void Execute(World world) { }
    }

    public class Unknown : IAction {
        public Unknown(string verb) => _verb = verb;
        public void Execute(World world) => Console.WriteLine(Data.Unknown.With(_verb));
        private readonly string _verb;
    }
    public class Echo : IAction {
        public Echo(string[]? echoed) => _echoed = echoed;
        public void Execute(World world) {
            Console.WriteLine(_echoed == null ? Data.Echo.What : Data.Echo.With(string.Join(" ", _echoed)));
        }
        private readonly string[]? _echoed;
    }
}
