#! /bin/bash

set -e

# ensure tz repository

echo "Checking tz repository..."
if [ -d tz/.git ]; then
    git -C tz checkout main
    git -C tz pull || true
else
    git clone https://github.com/eggert/tz.git
fi

# check latest (or passed in) version of tz

version="${version:-$(git -C tz describe --tags --abbrev=0)}"
dist_dir="$(pwd -P)/dist/$version"

# make tz data

if [ -d "$dist_dir" ]; then
    echo "Using existing build at '$dist_dir'."
else
    git -C tz -c advice.detachedHead=false checkout $version
    echo "Compiling tz data..."
    make -C tz --quiet install DESTDIR="$dist_dir" TZDIR="" REDO=posix_only
    git -C tz checkout main
fi

# build file

input="template.elm"
output="src/TimeZone/Abbreviation.elm"

echo "Preparing to build..."
cargo build --release

echo "Creating file $output for version $version..."
cat $input | cargo run --quiet --release -- --version "$version" --data-dir "$dist_dir" > $output
elm make $output
elm-format --yes $output
elm-verify-examples --run-tests
elm-test
