namespace btagsil
{
    public class World
    {
        public IPlayer Player { get; set; } = new Player(Data.Player.StartingLocation, Data.Player.StartingWeapon);
        public Dictionary<string, ILocation> Locations = new()
        {
            ["forest"] = new Forest(),
            ["square"] = new Square(),
            ["weapon shop"] = new WeaponShop(),
            ["cave"] = new Cave(),
        };
        public ILocation CurrentLocation { get { return Locations[Player.CurrentLocation]; } }
    }
}
