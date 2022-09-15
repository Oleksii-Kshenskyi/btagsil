# BTAGSIL, a Bullshit Text Adventure Game

## A surface level description

BTAGSIL is a Bullshit Text Adventure Game where Sky Is the Limit. Its IPA pronounciation is /biːˈtæɡsɪl/, or simply bee-tag-sil (sil as in silk), with the emphasis on "tag".

First things first, why the B? Well, there are a few reasons.
- It's never guaranteed at any point that this game is going to work as intended :) I don't just mean this as a warning, there's also the fact that I'm planning to make this project a long-term have-fun labor-of-love no-strings-attached kind of endeavor, so in the future once this project is of a relatively massive size, I expect all kinds of "fun accidents" to start happening :) 
- This game is a MASSIVE experiment. I want to write most of it myself from scratch, and the idea is to come up with all sorts of crazy ideas that are possible to implement within the concept of a text adventure game... and then to try implementing them.
- The technologies, the language, the architecture used for the game are not guaranteed at any pont. For now it's Rust with a minimum of dependencies. However, due to the fact that Rust is sometimes extremely obnoxious to write big applications in (compiler errors, borrow checker, lifetimes, I'm looking at you) I may start thinking about switching to something else at some point down the line.
- In general, BTAGSIL is in the embryo stage of development. Nothing is set in stone yet, no architecture is defined, nothing is guaranteed. Therefore, one should reasonably expect the unexpected :)

Secondly, and just to elaborate a bit, this is planned to be a long-term project. I don't expect to be able to develop it daily, but I do expect to be always returning to it and developing additional features. I'm planning to use GitHub milestones + issues for now to roughly outline the next set of features to work on. However, in the initial "there's nothing ready" stage of development (at least until 0.3.0 or so?) I'm not planning to use milestones/issues religiously and dilligently, rather as a convenience than anything else. Once (at the very least) the basic architecture is in place though, I'll start being a bit more meticulous about documenting everything.

## Building and playing

You should expect this to be as easy as

`cargo run --release`

as it usually is with the Rust projects. Hopefully I'll be able to make it work on most if not all the terminals and OSes Rust compiles on. As I'm not planning to be using any advanced terminal features like ASCII graphics or coloring (for now), I expect the game to be pretty portable across different platforms.

## Licensing

This is licensed under the standard MIT license. Provided you include the copyright notice, you can use this code however you like. 