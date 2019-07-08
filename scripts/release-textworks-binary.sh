#!/bin/bash

# creates a GitHub release (draft) and adds pre-built artifacts to the release
# after running this script user should manually check the release in GitHub, optionally edit it, and publish it

# args: :version_number (the version number of this release), :body (text describing the contents of the tag)
# example usage: ./gh_release_bamboo.sh "1.0.0" "Release notes: ..."
# => name: nRF5-ble-driver_<platform_name>_1.0.0_compiled-binaries.zip example: nRF5-ble-driver_win-64_2.0.1_compiled-binaries.zip

# to ensure that bash is used: https://answers.atlassian.com/questions/28625/making-a-bamboo-script-execute-using-binbash
if [ "$(ps -p "$$" -o comm=)" != "bash" ]; then
    bash "$0" "$@"
    exit "$?"
fi

# define variables
GH_ACCESS_TOKEN="9d103742269e07343b28b56c9f939e3303f3a05c"
GH_API="https://api.github.com"
GH_OWNER="iesl"
GH_REPO="$GH_API/repos/$GH_OWNER/watr-works/releases?access_token=$GH_ACCESS_TOKEN"

# create a GitHub release, see: https://www.barrykooij.com/create-github-releases-via-command-line/
RELEASE_JSON=$(printf '{"tag_name": "v%s","target_commitish": "master","name": "nRF5-ble-driver_%s","body": "%s","draft": true,"prerelease": false}' "$1" "$1" "$2")
echo "Creating a release with info: $RELEASE_JSON."

# release_id="$(curl --data "$RELEASE_JSON" $GH_REPO | python -c "import sys, json; print json.load(sys.stdin)['id']")"

# # exit script if GitHub release was not successfully created
# if [ -z "$release_id" ]
# then
#       echo "Failed to create GitHub release. Exiting with error."
#       exit 1
# else
#       echo "Release created with id: $release_id."
# fi

# # add artifacts to the release
# # note that we expect artifacts for each platform to be located in separate sub-dirs,
# # and we add these zipped sub-dirs to the release
# ARTIFACTS_DIR="./artifacts"
# for path in "$ARTIFACTS_DIR"/*; do
#     # if not a directory, skip
#     [ -d "$path" ] || continue

#     dirname="$(basename "$path")"

#     # create a zip file if windows release, else create a tar.gz file
#     fname="$ARTIFACTS_DIR/nRF5-ble-driver_${dirname}_$1_compiled-binaries"
#     if echo "$dirname" | grep -q "win"; then
#         f="$fname.zip"
#         # `-Z store` specifies no compression to be sure zip file is windows compatible: https://superuser.com/questions/5155/how-to-create-a-zip-file-compatible-with-windows-under-linux)
#         zip -Z store -r "$f" "$path"
#     else
#         f="$fname.tar.gz"
#         tar -czf "$f" "$path"
#     fi

#     GH_ASSET="https://uploads.github.com/repos/$GH_OWNER/pc-ble-driver/releases/$release_id/assets?name=$(basename "$f")&access_token=$GH_ACCESS_TOKEN"
#     curl --data-binary @"$f" -H "Content-Type: application/octet-stream" "$GH_ASSET"
# done

# echo "Release successful!"
# exit 0
