namespace btagsil
{
    public interface IObject {
        public string Name { get; }
        public string Description { get; }
        public Dictionary<string, object> Properties { get; }
    }
    public class Guard : IObject {
        public string Name { get; } = "guard";
        public string Description { get; } = "a big muscular man in heavy armor wielding a halberd.\nHe looks friendly and loves talking to strangers visiting the square";
        public Dictionary<string, object> Properties { get; } = new()
        {
            ["talks"] = new string[] {
                "How's your day going today, Your Highness?",
                "Did you know that dragons can fly?",
                "I've been thinking about the purpose of my life a lot recently... Why am I here?",
                "I want to kiss a girl so much...",
                "I'm a bit under the weather today, apologies.",
                "You look lovely today, Your Highness!",
                "WOW! The hat REALLY suits you! So stylish!",
                "It's important to remember to brush your teeth every morning.",
                "A bottle of fine ale would hit the spot right about now...",
                "It's impressive how quickly these tourists litter the square. Ugh.",
                "Did you know there's a fine weapon shop just nearby? Try going there!",
                "Are you tired?",
                "I remember that time I was a wee little lad..."
            }
        };
    }

    public interface ILocation {
        public string Name { get; }
        public string Description { get; }
        public string[] Connected { get; }
        public IObject[] Objects { get; }
    }
    public class Forest : ILocation {
        public string Name { get; } = "forest";
        public string Description { get; } = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside.";
        public string[] Connected { get; } = { "square" };
        public IObject[] Objects { get; } = Array.Empty<IObject>();
    }
    public class Square : ILocation {
        public string Name { get; } = "square";
        public string Description { get; } = "full of people having fun and minding their business.\nA huge muscular man in heavy armor catches your eye.";
        public string[] Connected { get; } = { "forest" };
        public IObject[] Objects { get; } = new IObject[] { new Guard() };
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
            public static string YouCanGoTo(string destinations) => $"You can go to {destinations} from here.";
        }
        public static class Go {
            public static string ForgotTo => "You can only go 'to' places.";
            public static string ToWhere => "Go to... where?";
            public static string Unknown => "Where are you trying to go?\nTry 'go to <place>'.\nTry 'where can i go' to see where you can go from here.";
            public static string AlreadyThere(string location) => $"You can't go to {location}, you're already there!";
            public static string WrongLocation(string location) => $"You can't go to {location}, it doesn't exist!";
            public static string NotConnected(string from, string to) => $"You can't go from {from} to {to}, they're not connected.\nTry 'where can i go' to see where you can go from here.";
            public static string Unreachable => "GoTo.Execute(): UNREACHABLE: location doesn't exist but is connected?!";
            public static string WentTo(string location) => $"You went to {location}.";
        }
        public static class Look {
            public static string OnlyAtStuff => "You can only look 'at' stuff.";
            public static string AtWhat => "Look at... what?";
            public static string Unknown => "You're trying to look for something?\nTry 'look at <object>' or 'look around'.";
            public static string NothingAround => "You don't see anything important here.";
            public static string SeeObjects(string objects) => $"You see {objects} here.";
            public static string DontSeeObject(string name) => $"You don't see any {name}s around here.";
            public static string DescribeObject(string name, string descr) => $"You see the {name}. It's {descr}.";
        }
    }
}
