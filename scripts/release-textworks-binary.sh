#!/bin/bash


sbt compile release

source ./set-release-versions

echo ghr $VER text-works/target/universal/textworks-$VERSION_STR.tgz
