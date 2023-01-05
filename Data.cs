namespace btagsil
{
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
    public class Axe : IWeapon {
        public string Name { get; } = "axe";
        public string Description { get; } = "a humongous razor-sharp double-headed greataxe";
    }
    public class Sword : IWeapon {
        public string Name { get; } = "sword";
        public string Description { get; } = "a gorgeous long ornamented claymore";
    }
    public class Bow : IWeapon {
        public string Name { get; } = "bow";
        public string Description { get; } = "a gigantic greatbow that uses spears as arrows";
    }

    public interface IObject {
        public string Name { get; }
        public string Description { get; }
        public Dictionary<string, object> Properties { get; }
    }
    public class Dummy : IObject {
        public string Name { get; } = "dummy";
        public string Description { get; } = "dummy";
        public Dictionary<string, object> Properties { get; } = new();
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
            },
            ["talkMethod"] = new Func<string[], string> ((lines) => {
                Random rand = new();
                return lines[rand.Next(0, lines.Length)];
            })
        };
    }
    public class Shopkeeper : IObject {
        public string Name { get; } = "shopkeeper";
        public string Description { get; } = "an old man with an eye for trade.\nHe's throwing glances at you hoping you'll buy something from him";
        public Dictionary<string, object> Properties { get; } = new()
        {
            ["talks"] = new string[] { "Stop talking and buy something already, you flirtatious vagabond!" },
            ["talkMethod"] = new Func<string[], string>((lines) => lines[0]),
            ["sells"] = new IWeapon[] { new Axe(), new Sword(), new Bow()},
        };
    }
    public class Monster : IObject {
        public string Name { get; } = "monster";
        public string Description { get; } = "a chilling monstrosity. It has briars and spikes all over its hardened skin.\nIt clearly doesn't like you";
        public Dictionary<string, object> Properties { get; } = new();
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
        public string[] Connected { get; } = { "forest", "weapon shop", "cave" };
        public IObject[] Objects { get; } = new IObject[] { new Guard() };
    }
    public class WeaponShop : ILocation {
        public string Name { get; } = "weapon shop";
        public string Description { get; } = "crammed with fine quality weapons to buy.\nAn axe, a sword and a bow catch your eye.";
        public string[] Connected { get; } = { "square" };
        public IObject[] Objects { get; } = new IObject[] { new Shopkeeper() };
    }
    public class Cave : ILocation {
        public string Name { get; } = "cave";
        public string Description { get; } = "chilly and dark in here and the silence seems almost deafening.\nAlthough, wait... Was that a growl?";
        public string[] Connected { get; } = { "square" };
        public IObject[] Objects { get; } = new IObject[] { new Monster() };
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
            public static string AtWeapon(string descr) => $"You see {descr}.";
        }
        public static class Talk {
            public static string OnlyToStuff => "You can only talk 'to' someone.";
            public static string ToWho => "Talk to... who?";
            public static string Unknown => "Are you trying to talk to someone?\nTry 'talk to <object>.\nTry 'look around' to see who's around to talk to.";
            public static string EntityDoesNotExist(string name) => $"You don't see any {name}s around to talk to.";
            public static string EntityDoesNotTalk(string name) => $"The {name} doesn't think talking to you is particularly fun.";
            public static string ToEntity(string name, string line) => $"The {name} says: '{line}'";
        }
        public static class What {
            public static string TheWhat => "What 'what'?";
            public static string IsWhat => "What is... what?";
            public static string DontKnowWhatIs(string thing) => $"No clue what '{thing}' is ¯\\_(ツ)_/¯";
            public static string Unknown => "What are you trying to ask?\nTry 'what can i buy' to see what you can buy around here.";
            public static string NoOneIsSelling => "No one seems to be interested in selling you stuff around here.";
            public static string PurchaseOptions(string things) => $"You can buy {things} here.";
        }
        public static class Buy {
            public static string WhatFromWho => "Buy... what? And who to buy that from?";
            public static string Unknown => "Are you trying to buy something?\nTry 'what can i buy' to see what you can buy around here.\nTry 'buy <thing> from <seller>' to buy something from someone.";
            public static string ThingFromWho(string thing) => $"Do you want to buy {thing}?\nWho do you want to buy that from?";
            public static string StillNoSeller(string thing) => $"Buy {thing} from... who?";
            public static string SellerDoesNotExist(string seller) => $"You don't see any {seller}s around to sell you stuff.";
            public static string SellerDoesNotSell(string seller) => $"The {seller} doesn't seem particularly excited about the idea of selling you stuff.";
            public static string SellerDoesNotSellThing(string thing, string sellerName) => $"The {sellerName} does not have any {thing}s to sell you.";
            public static string ThanksForPurchase(string thing, string sellerName) => $"The {sellerName} says: 'Thanks for buying the {thing}! Come back anytime!'";

        }
    }
}
