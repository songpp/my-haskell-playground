#! /bin/bash

THIS_PROJECT=$(cd "$(dirname "$0")"; pwd);
THIS_PROJECT_DIR_NAME=$(basename "$THIS_PROJECT");
MY_SANDBOX=~/tools/dev-sandbox/;

set -x;
exec 3>&1
start () {
    hsdev start --port 4567 \
        --cache "$THIS_PROJECT/.hsdev-cache" \
        >&3;
    sleep 1;
}

scan () {
    cd $THIS_PROJECT;
    hsdev scan --cabal --project "../$THIS_PROJECT_DIR_NAME"  \
        --sandbox $MY_SANDBOX >&3;
}

loadCache() {
    cd $THIS_PROJECT;
    hsdev load --cache-dir "$THIS_PROJECT/.hsdev-cache" >&3;
}

case "$1" in
    load)
        $( loadCache )
    ;;
    *)
        $( start && scan )
    ;;
esac

exec 3>&-

# hsdev scan --cabal --project /Users/metaflower/dev/code/haskell/playground \
#    --sandbox /Users/metaflower/tools/dev-sandbox/

#
#  cabal sandbox init --sandbox /Users/metaflower/tools/dev-sandbox/
#  cabal configure
#  cabal install --only-deps
#  cabal repl


#  putStrLn . ppShow . runParse =<< testClassBytes
#

