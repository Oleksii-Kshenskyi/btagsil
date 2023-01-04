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
    public class LookAtWeapon : IAction {
        public string Execute(World world) => Data.Look.AtWeapon(world.Player.Weapon.Description);
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
    public class WhatWhat : IAction {
        public string Execute(World world) => Data.What.TheWhat;
    }
    public class WhatIsWhat : IAction {
        public string Execute(World world) => Data.What.IsWhat;
    }
    public class WhatIsDontKnow : IAction {
        public WhatIsDontKnow(string thing) => _thing = thing;
        public string Execute(World world) => Data.What.DontKnowWhatIs(_thing);
        private string _thing;
    }
    public class UnknownWhat : IAction {
        public string Execute(World World) => Data.What.Unknown;
    }
    public class WhatCanIBuy : IAction {
        public string Execute(World world)
        {
            var sellers = world.CurrentLocation.Objects.Where(o => o.Properties.ContainsKey("sells")).ToArray();
            return sellers.Length switch
            {
                0 => Data.What.NoOneIsSelling,
                1 => Data.What.PurchaseOptions(Utils.ArticledEnumeration(((IWeapon[]) sellers[0].Properties["sells"]).Select(w => w.Name).ToArray(), "the")),
                _ => throw new NotImplementedException("WhatCanIBuy: UNREACHABLE: more than one seller on location?!"),
            };
        }
    }

    public class BuyWhatFromWho : IAction {
        public string Execute(World world) => Data.Buy.WhatFromWho;
    }
    public class UnknownBuy : IAction {
        public string Execute(World world) => Data.Buy.Unknown;
    }
    public class BuyThingFromWho : IAction {
        public BuyThingFromWho(string thing) => _thing = thing;
        public string Execute(World world) => Data.Buy.ThingFromWho(_thing);
        private string _thing;
    }
    public class BuyStillNoSeller : IAction {
        public BuyStillNoSeller(string thing) => _thing = thing;
        public string Execute(World world) => Data.Buy.StillNoSeller(_thing);
        private string _thing;
    }
    public class BuyThingFromSeller : IAction {
        public BuyThingFromSeller(string thing, string sellerName) => (_thing, _sellerName) = (thing, sellerName);
        public string Execute(World world) {
            var sellers = world.CurrentLocation.Objects.Where(o => o.Name == _sellerName).ToArray();
            bool sellerExists = sellers.Length == 1;
            var seller = sellerExists ? sellers[0] : new Dummy();
            bool sellerSells = sellerExists && seller.Properties.ContainsKey("sells");
            bool sellerSellsThing = sellerSells && ((IWeapon[])seller.Properties["sells"]).Where(t => t.Name == _thing).ToArray().Length == 1;
            return new bool[] { sellerExists, sellerSells, sellerSellsThing } switch
            {
                [false, _, _] => Data.Buy.SellerDoesNotExist(_sellerName),
                [true, false, _] => Data.Buy.SellerDoesNotSell(_sellerName),
                [true, true, false] => Data.Buy.SellerDoesNotSellThing(_thing, _sellerName),
                [true, true, true] => SellThing(world),
                _ => throw new NotImplementedException($"BuyThingFromSeller: UNREACHABLE: trying to buy {_thing} from {_sellerName}: impossible?!"),
            };
        }
        private readonly string _thing;
        private readonly string _sellerName;
        private string SellThing(World world) {
            IObject theSeller = world.CurrentLocation.Objects.Where(o => o.Name == _sellerName).ToArray()[0];
            IWeapon theThing = ((IWeapon[])theSeller.Properties["sells"]).Where(w => w.Name == _thing).ToArray()[0];
            world.Player.Weapon = theThing;
            return Data.Buy.ThanksForPurchase(_thing, _sellerName);
        }
    }
}
