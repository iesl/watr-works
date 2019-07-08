#!/bin/bash

. "./gh-access-token.txt"
# define variables
GH_API="https://api.github.com"
GH_UPLOAD="https://uploads.github.com"
GH_OWNER="iesl"
GH_REPO="watr-works"
REPO_PATH="$GH_OWNER/$GH_REPO"

release_id="v0.11"

# GH_REPO="$GH_API/repos/$REPO_PATH/branches/acs-dev?access_token=$GH_ACCESS_TOKEN"
# GH_REPO="$GH_API/repos/$REPO_PATH/releases/v0.11/?access_token=$GH_ACCESS_TOKEN"
GH_REPO="$GH_API/repos/$REPO_PATH/releases/latest?access_token=$GH_ACCESS_TOKEN"

# GET "$GH_REPO"

TW="textworks-app.tar.gz"
# # tar -czf "$f" "$path"

# GH_ASSET="$GH_UPLOAD/repos/$REPO_PATH/releases/$release_id/assets?name=$TW&access_token=$GH_ACCESS_TOKEN"
GH_ASSET="https://uploads.github.com/repos/iesl/watr-works/releases/18480819/assets?name=$TW&access_token=$GH_ACCESS_TOKEN"
# echo "$GH_ASSET"

curl --data-binary "@$TW" -H "Content-Type: application/octet-stream" "$GH_ASSET"

# {
#   "url": "https://api.github.com/repos/iesl/watr-works/releases/assets/13606651",
#   "id": 13606651,
#   "node_id": "MDEyOlJlbGVhc2VBc3NldDEzNjA2NjUx",
#   "name": "textworks-app.tar.gz",
#   "label": "",
#   "browser_download_url": "https://github.com/iesl/watr-works/releases/download/v0.11/textworks-app.tar.gz"
# }
