# BTAGSIL, a Bullshit Text Adventure Game

## A surface level description

BTAGSIL is a Bullshit Text Adventure Game where Sky Is the Limit. Its IPA pronounciation is /biːˈtæɡsɪl/, or simply bee-tag-sil (sil as in silk), with the emphasis on "tag".

First things first, why the B? Well, there are a few reasons.
- It's never guaranteed at any point that this game is going to work as intended :) I don't just mean this as in "expect many bugs", there's also the fact that I'm planning to make this project a long-term have-fun labor-of-love no-strings-attached kind of endeavor, so in the future once this project is of a relatively massive size, I expect all kinds of "fun accidents" to start happening :) 
- This game is a MASSIVE experiment. I want to write the most important systems myself from scratch, and the point is to come up with all sorts of crazy ideas that are possible to implement within the concept of a text adventure game... and then to try implementing them.
- The technologies, the language, the architecture used for implementing the game are not guaranteed at any point. At the moment it's Python. However, this project was originally started in Rust, then switched to C++, and eventually to Python. Also, the [Language Feast Milestone](https://github.com/Oleksii-Kshenskyi/btagsil/milestone/3) featured 7 short BTAGSIL prototypes (each of them written in a different language), each of them implemented to varying degrees of completeness, so therefore this project is experimentation incarnate and is not supposed to be stable and predictable (yet).
- In general, BTAGSIL is in the embryo stage of development. Nothing is set in stone yet, no architecture is defined, nothing is guaranteed. Therefore, one should reasonably expect the unexpected :)
- As of right now, I'm in the prototyping stage of the project (that's the biggest reason for using Python for now). As I'm not really familiar with text games and I would also like to use this prototype to see what would be possible and fun for me to work on within the confines of this genre, so the main point of the current stage to quickly iterate on adding a few basic concepts/mechanics to the game in Python to figure out what works and what doesn't. Once this prototyping stage is over, I'll be able to make more informed decisions on which language/technology to use, which mechanics/concepts to implement and what setting/world/concept fits these mechanics the best.

Secondly, and just to elaborate a bit, this is planned to be a long-term project. I don't expect to be able to develop it daily, but I do expect to be always returning to it and developing additional features. I'm planning to use GitHub milestones + issues for now to roughly outline the next set of features to work on. However, in the initial "there's nothing ready" stage of development (at least until 0.3.0 or so?) I'm not planning to use milestones/issues religiously and dilligently, rather as a convenience than anything else. Once (at the very least) the basic architecture is in place though, I'll start being a bit more meticulous about documenting everything.

You can find what's currently being worked on in the current milestone, [0.1.0](https://github.com/Oleksii-Kshenskyi/btagsil/milestone/1).

## How versioning works

For now, I'll mostly be incrementing the minor version (the second number) for each new release. Each minor version increment is going to be its own milestone that features quite a lot of new features implemented, a couple new systems defined, a few new things to do etc. 

0.1.0 is the first milestone / minor version being worked on, and it's going to define the basics of how BTAGSIL works, what the world contains and how it is generated, as well as a couple basic in-game systems such as the command/action system (how the user inputs commands for the game to interpret and how the game interprets them), the UI, as well as some basic documentation and unit testing efforts.

The second release planned, 0.2.0, is long loooong time away, but for now it's planned to feature the simulation system (commands given to BTAGSIL not as user input in real time but as essentially a script, a text file with a bunch of commands to run, and the simulation executable's job is going to be to simulate how the world would look like after all of the commands in the script are implemented), probably a more flexible content generation system (the content of the game should be stored in text template files instead of being hardcoded in the code) and some additional content for more things to do.

As for 0.3.0, it's too early to even theorize what could end up there.

## Building and playing

As for now (the almost-empty Python prototype stage), I'll be using the "main Python file in the root of the repo plus a bunch of implementation details in the `game` folder" kind of structure. Probably with a separate `tests` folder. So in order to run the game, just use the `python` executable to run the `sky.py` file in the root of the repo.

## Licensing

This is licensed under the standard MIT license. Provided you include the copyright notice, you can use this code however you like. The licensing terms for this project may change in the future, nothing is set in stone yet.
