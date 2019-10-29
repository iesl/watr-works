#!/bin/bash

sbt clean compile "textworks/universal:packageZipTarball" release

source ./set-release-versions

echo ghr $VERSION_STR text-works/target/universal/textworks-$VERSION_STR.tgz



