namespace btagsil
{
    public static class Utils {
        public static string ArticledEnumeration(string[] items, string article) =>
            string.Join(", ", items.Select(s => article + " " + s));
    }
    public interface IAction
    {
        string Execute(World world);
    }

    public class Exit : IAction {
        public Exit() { }
        public string Execute(World world) {
            Console.Write($"{Data.Exit.Message}\n\n");
            Environment.Exit(0); return "";
        }
    }

    public class Empty : IAction {
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
    public class WhereCanIGo : IAction {
        public string Execute(World world) =>
            Data.Where.YouCanGoTo(Utils.ArticledEnumeration(world.CurrentLocation.Connected, "the"));
    }
    public class UnknownWhere : IAction {
        public string Execute(World world) => Data.Where.Unknown;
    }
    public class GoForgotTo : IAction {
        public string Execute(World world) => Data.Go.ForgotTo;
    }
    public class GoToWhere : IAction {
        public string Execute(World world) => Data.Go.ToWhere;
    }
    public class UnknownGo : IAction {
        public string Execute(World world) => Data.Go.Unknown;
    }
    public class GoTo : IAction {
        public GoTo(string location) => _location = location;
        public string Execute(World world) {
            ILocation curLoc = world.CurrentLocation;
            bool alreadyThere = (_location == curLoc.Name);
            bool locationExists = world.Locations.ContainsKey(_location);
            bool locationIsConnected = curLoc.Connected.Contains(_location);
            return new bool[] { alreadyThere, locationExists, locationIsConnected } switch
            {
                [true, _, _] => Data.Go.AlreadyThere(_location),
                [_, false, false] => Data.Go.WrongLocation(_location),
                [_, _, false] => Data.Go.NotConnected(world.CurrentLocation.Name, _location),
                [_, true, true] => WentTo(world, _location),
                [_, false, true] => throw new NotImplementedException(Data.Go.Unreachable),
                _ => throw new NotImplementedException()
            };
        }

        private string WentTo(World world, string location) {
            world.Player.CurrentLocation = location;
            return Data.Go.WentTo(location);
        }
        private string _location;
    }

    public class LookOnlyAtStuff : IAction {
        public string Execute(World world) => Data.Look.OnlyAtStuff;
    }
    public class LookAtWhat : IAction {
        public string Execute(World world) => Data.Look.AtWhat;
    }
    public class UnknownLook : IAction {
        public string Execute(World world) => Data.Look.Unknown;
    }
    public class LookAround : IAction {
        public string Execute(World world) => world.CurrentLocation.Objects.Select(o => o.Name).ToArray() switch {
            [] => Data.Look.NothingAround,
            var objs => Data.Look.SeeObjects(Utils.ArticledEnumeration(objs, "the"))
        };
    }
    public class LookAt : IAction {
        public LookAt(string objectName) => _objectName = objectName;
        public string Execute(World world) => world.CurrentLocation.Objects.Where(o => o.Name == _objectName).ToArray() switch
        {
            [] => Data.Look.DontSeeObject(_objectName),
            [var obj] => Data.Look.DescribeObject(obj.Name, obj.Description),
            _ => throw new NotImplementedException($"LookAt: UNREACHABLE: more than one object with the name {_objectName}?!")
        };
        private readonly string _objectName;
    }
    public class TalkOnlyToStuff : IAction {
        public string Execute(World world) => Data.Talk.OnlyToStuff;
    }
    public class TalkToWho : IAction {
        public string Execute(World world) => Data.Talk.ToWho;
    }
    public class UnknownTalk : IAction {
        public string Execute(World world) => Data.Talk.Unknown;
    }
    public class TalkToEntity : IAction {
        public TalkToEntity(string entityName) => _entityName = entityName;
        public string Execute(World world) {
            var found = world.CurrentLocation.Objects.Where(o => o.Name == _entityName).ToArray();
            bool entityExists = found.Length == 1;
            var entity = entityExists ? found[0] : null;

            bool entityTalks = entityExists && found[0].Properties.ContainsKey("talks");
            var lines = entityTalks ? (string[]) found[0].Properties["talks"] : Array.Empty<string>();
            var talkMethod = entityTalks ? (Func<string[], string>) found[0].Properties["talkMethod"] : (s) => "";
            return new bool[] { entityExists, entityTalks } switch {
                [false, false] => Data.Talk.EntityDoesNotExist(_entityName),
                [true, false] => Data.Talk.EntityDoesNotTalk(_entityName),
                [true, true] => Data.Talk.ToEntity(_entityName, talkMethod(lines)),
                [false, true] => throw new NotImplementedException($"TalkToEntity: UNREACHABLE: {_entityName} doesn't exist but talks?!"),
                _ => throw new NotImplementedException()
            };
        }
        private readonly string _entityName;
    }
}
