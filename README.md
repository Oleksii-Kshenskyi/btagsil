# BTAGSIL, a Bullshit Text Adventure Game

## A surface level description

BTAGSIL is a Bullshit Text Adventure Game where Sky Is the Limit. Its IPA pronounciation is /biːˈtæɡsɪl/, or simply bee-tag-sil (sil as in silk), with the emphasis on "tag".

First things first, why the B? Well, there are a few reasons.
- It's never guaranteed at any point that this game is going to work as intended :) I don't just mean this as in "expect many bugs", there's also the fact that I'm planning to make this project a long-term have-fun labor-of-love no-strings-attached kind of endeavor, so in the future once this project is of a relatively massive size, I expect all kinds of "fun accidents" to start happening :) 
- This game is a MASSIVE experiment. I want to write the most important systems myself from scratch, and the point is to come up with all sorts of crazy ideas that are possible to implement within the concept of a text adventure game... and then to try implementing them.
- The technologies, the language, the architecture used for implementing the game are not guaranteed at any point. At the moment it's Rust. However, this project was originally started in Rust, then switched to C++, and eventually to Python, and to Rust AGAIN! Also, the [Language Feast Milestone](https://github.com/Oleksii-Kshenskyi/btagsil/milestone/3) featured 7 short BTAGSIL prototypes (each of them written in a different language), each of them implemented to varying degrees of completeness, so therefore this project is experimentation incarnate and is not supposed to be stable and predictable (yet).
- In general, BTAGSIL is in the embryo stage of development. Nothing is set in stone yet, no architecture is defined, nothing is guaranteed. Therefore, one should reasonably expect the unexpected :)
- As of right now, I'm in the prototyping stage of the project. As I'm not really familiar with text games and I would also like to use this prototype to see what would be possible and fun for me to work on within the confines of this genre, so the main point of the current stage to quickly iterate on adding a few basic concepts/mechanics to the game to figure out what works and what doesn't. Once this prototyping stage is over, I'll be able to make more informed decisions on which language/technology to use, which mechanics/concepts to implement and what setting/world/concept fits these mechanics the best.

Secondly, and just to elaborate a bit, this is planned to be a long-term project. I don't expect to be able to develop it daily, but I do expect to be always returning to it and developing additional features. I'm planning to use GitHub milestones + issues for now to roughly outline the next set of features to work on. Milestones and versioning are canceled for now, as they distract me from what I want to actually do - experiment, learn, and have fun. Therefore, we're 100% GitHub issues right now. To figure out what I'm working on right now, look for last commits and issues with the "in progress" label. However, in the initial "there's nothing ready" stage of development (until the concepts and technologies are solidified) I'm not planning to use milestones/issues religiously and dilligently, rather as a convenience than anything else. Once (at the very least) the basic architecture is in place though, I'll start being a bit more meticulous about documenting everything.

## Building and playing

As for now (the almost-empty Rust prototype stage), you can build and run this as any average Rust project, with `cargo run --release` for example. Once the architecture is a bit more complicated, we'll see if I need to add any additional stages to the build/run process.

## Licensing

This is licensed under the standard MIT license. Provided you include the copyright notice, you can use this code however you like. The licensing terms for this project may change in the future, nothing is set in stone yet. It will not be closed source or any sort of restrictive license though, this project is intended as open software that's free for others to experiment and have fun with.
