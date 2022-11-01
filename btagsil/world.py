import random as r

import btagsil.data as data
from btagsil.data import World, Player, Weapon, Object, Location, Forest, Square, WeaponShop, Cave

# Utility functions

def articled_enumeration(things: list, article: str) -> str:
    return ', '.join(map(lambda x: article + ' ' + x, things))

def object_has_prop(loc: Location, object_name: str, prop: str) -> bool:
    object_exists = object_name in loc.objects
    the_object = loc.objects[object_name] if object_exists else None
    object_has_the_prop = prop in the_object.properties if object_exists else False
    return object_has_the_prop

def take_out_of_pandoras_box(the_box: list[str]) -> str:
    return r.choice(the_box)

# Working with locations

def get_current_loc(world: World) -> Location:
    return world.locations[world.player.current_location]

def current_loc_description(world: World) -> str:
    current_loc = get_current_loc(world)
    descr = current_loc.description
    name = current_loc.name
    return data.describe_loc(name, descr)

def possible_destinations(world: World) -> str:
    current_loc = get_current_loc(world)
    destinations_text = articled_enumeration(current_loc.connected, 'the')
    return data.you_can_go_to(destinations_text)

# Navigational actions

def look_around(world: World) -> str:
    object_names = list(get_current_loc(world).objects.keys())
    match object_names:
        case []: return data.nothing_to_look_at()
        case names: return data.look_around(articled_enumeration(names, 'a'))

def look_at_object(world, what):
    object_name = ' '.join(what)
    cur_objects = get_current_loc(world).objects
    object_exists = object_name in cur_objects

    if object_exists:
        the_object = cur_objects[object_name]
        return data.look_at_object(the_object.name, the_object.description)
    else:
        return data.no_object_around(object_name)

def talk_to_guard(the_talker: Object) -> str:
    guard_says = take_out_of_pandoras_box(the_talker.behavior["pandora's box"])
    return data.entity_says(the_talker.name, guard_says)

def talk_to_shopkeeper(the_talker: Object) -> str:
    return data.entity_says(the_talker.name, data.shopkeeper_line())

def choose_talker_and_talk(world: World, entity: str) -> str:
    the_talker = get_current_loc(world).objects[entity]
    match entity:
        case "guard": return talk_to_guard(the_talker)
        case "shopkeeper": return talk_to_shopkeeper(the_talker)
        case _: ValueError(f"world.choose_talker_and_talk: Unknown talker '{entity}'")

def talk_to(world: World, entity: list[str]) -> str:
    current_loc = get_current_loc(world)
    current_objects = list(current_loc.objects.keys())
    object_name = ' '.join(entity)
    object_exists = object_name in current_objects
    object_talks = object_has_prop(current_loc, object_name, "talks")
    match [object_exists, object_talks]:
        case [True, True]: return choose_talker_and_talk(world, object_name)
        case [True, False]: return data.object_doesnt_talk(object_name)
        case [False, False]: return data.talker_object_doesnt_exist(object_name)
        case [False, True]: raise ValueError("world.talk_to(): UNREACHABLE: an object that doesn't exist but talks?!")

def get_purchase_options(seller: Object) -> str:
    options = list(seller.behavior["sells"].keys())
    return data.you_can_buy_this(articled_enumeration(options, 'the'))

def purchase_options(world: World) -> str:
    current_loc = get_current_loc(world)
    current_objects = current_loc.objects.values()
    sellers = list(filter(lambda o: object_has_prop(current_loc, o.name, "sells"), current_objects))
    match len(sellers):
        case 0: return data.no_one_is_selling()
        case 1: return get_purchase_options(sellers[0])
        case _: raise ValueError("world.purchase_options(): UNREACHABLE: more than 1 seller at current location?!")


# Actions that mutate the World

# the attack action

def assign_new_hp(world: World, the_target: Object, player_new_hp: int, target_new_hp: int) -> None:
    world.player.hp = player_new_hp
    the_target.behavior["hp"] = target_new_hp

def perform_mutual_smacking(world: World, the_target: Object, player_new_hp: int, target_new_hp: int) -> str:
    assign_new_hp(world, the_target, player_new_hp, target_new_hp)
    return data.mutually_smack(world.player.weapon.swing, player_new_hp, the_target.name, the_target.behavior["swing"], target_new_hp)

def you_win(world: World, the_target: Object, player_new_hp: int, target_new_hp: int) -> str:
    return perform_mutual_smacking(world, the_target, player_new_hp, target_new_hp) + '\n' + data.target_dead(the_target.name)

def you_are_ded(target_name: str, target_swing: str) -> None:
    print(data.you_are_ded(target_name, target_swing) + '\n')
    exit(0)

def check_who_dies_and_smack(world: World, the_target: Object) -> str:
    player_hp = world.player.hp
    player_damage = world.player.weapon.damage
    target_hp = the_target.behavior["hp"]
    target_damage = the_target.behavior["damage"]

    player_new_hp = player_hp - target_damage
    target_new_hp = target_hp - player_damage

    does_target_die = target_new_hp <= 0
    does_player_die = player_new_hp <= 0
    
    match [does_player_die, does_target_die]:
        case [False, False]: return perform_mutual_smacking(world, the_target, player_new_hp, target_new_hp)
        case [True, _]: return you_are_ded(the_target.name, the_target.behavior["swing"])
        case [False, True]: return you_win(world, the_target, player_new_hp, target_new_hp)

def smack(world: World, target: str) -> str:
    current_loc = get_current_loc(world)
    current_objects = current_loc.objects

    does_target_exist = target in current_objects
    the_target = current_objects[target] if does_target_exist else None
    does_target_fight = object_has_prop(current_loc, target, "fights") if the_target else False
    is_target_already_dead = the_target.behavior["hp"] <= 0 if does_target_fight else False

    match [does_target_exist, does_target_fight, is_target_already_dead]:
        case [True, True, False]: return check_who_dies_and_smack(world, the_target)
        case [True, True, True]: return data.stop_it_its_already_dead(target)
        case [False, _, _]: return data.no_objects_to_smack(target)
        case [_, False, _]: return data.bad_idea_to_smack(target)
        case _: ValueError(f"world.smack(): UNREACHABLE: unknown fight state: {[does_target_exist, does_target_fight, is_target_already_dead]}!")

# The buy action

def perform_purchase(world: World, the_thing: Weapon, the_object: Object) -> str:
    world.player.weapon = the_thing
    return data.thanks_for_purchase(the_object.name, the_thing.name)

def buy_thing_from_seller(world: World, thing: str, seller: list[str]) -> str:
    seller_name = ' '.join(seller)
    current_loc = get_current_loc(world)
    current_objects = current_loc.objects
    seller_exists = seller_name in current_objects
    can_buy_from_seller = object_has_prop(current_loc, seller_name, "sells") if seller_exists else False
    the_object = current_loc.objects[seller_name] if seller_exists else None
    assortment = the_object.behavior["sells"] if can_buy_from_seller else None
    thing_is_sold = thing in assortment if can_buy_from_seller else False
    the_thing = assortment[thing] if thing_is_sold else None

    match [seller_exists, can_buy_from_seller, thing_is_sold]:
        case [False, _, _]: return data.seller_does_not_exist(seller_name)
        case [_, False, _]: return data.seller_doesnt_sell(seller_name)
        case [_, _, False]: return data.seller_doesnt_sell_thing(seller_name, thing)
        case _: return perform_purchase(world, the_thing, the_object)

# The go action

def move_to_destination(world: World, loc_name: str) -> str:
    world.player.current_location = loc_name
    return data.you_went_to(loc_name)

def move_to(world: World, where: list[str]) -> str:
    loc_name = ' '.join(where)
    current_loc_name = world.player.current_location
    connected = get_current_loc(world).connected

    already_there = loc_name == current_loc_name
    no_such_loc = loc_name not in world.locations
    not_connected = loc_name not in connected

    match [already_there, no_such_loc, not_connected]:
        case [True, _, _]: return data.already_there(loc_name)
        case [_, True, _]: return data.no_such_loc(loc_name)
        case [_, _, True]: return data.cant_go_there_from_here(loc_name, current_loc_name)
        case _: return move_to_destination(world, loc_name)

# Initializing the World

def init_world() -> World:
    return World(player = Player(),
                 locations = {"forest": Forest(),
                              "square": Square(),
                              "weapon shop": WeaponShop(),
                              "cave": Cave()})