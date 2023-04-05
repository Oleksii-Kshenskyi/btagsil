# This script should work everywhere where there's a bash available (including Windows)
# Its main purpose is to introduce a few quick and easy shortcuts for building and configuring BTAGSIL
# You can source this script to make the shortcuts available in your bash:
# `source ./devenv.sh`

alias bfc="make fullclean"
alias blc="make localclean"
alias bcc="make cleancache"
alias bcm="make cmakegen"
alias bbl="make compile"
alias brn="make run"
alias brna="build/btagsil-cli" # functionally the same as `brn`, but can be used to pass CLI args to BTAGSIL
alias bsb="make sandbox"