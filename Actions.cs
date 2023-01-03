﻿namespace btagsil
{
    public static class Utils {
        public static string ArticledEnumeration(string[] items, string article) =>
            string.Join(", ", items.Select(s => article + " " + s));
    }
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
}
