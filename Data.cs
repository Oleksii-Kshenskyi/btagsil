namespace btagsil
{
    public interface ILocation {
        public string Name { get; }
        public string Description { get; }
    }
    public class Forest : ILocation {
        public string Name { get; } = "forest";
        public string Description { get; } = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside.";
    }
    public interface IWeapon
    {
        string Name { get; }
        string Description { get; }
    }
    public class BareFists : IWeapon
    {
        public string Name { get; } = "bare fists";
        public string Description { get; } = "just your bare fists";
    }
    public interface IPlayer
    {
        string CurrentLocation { get; set; }
        IWeapon Weapon { get; set; }
    }
    public class Player : IPlayer {
        public Player(string startLoc, IWeapon startWeapon) => (CurrentLocation, Weapon) = (startLoc, startWeapon);
        public string CurrentLocation { get; set; }
        public IWeapon Weapon { get; set; }
    }
    public static class Data
    {
        public static class Player {
            public static string StartingLocation { get; } = "forest";
            public static IWeapon StartingWeapon { get; } = new BareFists();
        }
        public static class System {
            public static string Prompt { get; } = "^_^ ~~>> ";
        }
        public static class Echo {
            public static string ZeroWordsError { get; } = "ParseEcho(): echo with 0 words?!";
            public static string What { get; } = "Echo what?";
            public static string With(string what) => $"'{what}'";
        }
        public static class Exit {
            public static string Message { get; } = "Thanks for playing! See you soon! ^_^";
        }
        public static class Unknown {
            public static string With(string what) => $"Ugh... How does one '{what}'?";
        }
        public static class Where {
            public static string What => "Where... what?";
            public static string Unknown => "What are you trying to ask?\nMaybe try 'where am i' or 'where can i go'?";
        }
    }
}
