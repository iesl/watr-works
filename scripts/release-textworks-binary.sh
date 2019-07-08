#!/bin/bash

# define variables
# GH_ACCESS_TOKEN="9d103742269e07343b28b56c9f939e3303f3a00"
GH_ACCESS_TOKEN="5f10c573fec1632bcb5cdb9276613ce91afb4a26"
GH_API="https://api.github.com"
GH_UPLOAD="https://uploads.github.com"
GH_OWNER="iesl"

release_id="v0.11"

# GH_REPO="$GH_API/repos/$GH_OWNER/watr-works/branches/branch/acs-dev/releases/latest?access_token=$GH_ACCESS_TOKEN"
# GH_REPO="$GH_API/repos/$GH_OWNER/watr-works/branches/acs-dev?access_token=$GH_ACCESS_TOKEN"
GH_REPO="$GH_API/repos/$GH_OWNER/watr-works/releases/tags/$release_id?access_token=$GH_ACCESS_TOKEN"

POST "$GH_REPO"


# f="./textworks-app.tar.gz"
# # tar -czf "$f" "$path"

# GH_ASSET="$GH_UPLOAD/repos/$GH_OWNER/watr-works/releases/$release_id/assets?name=$(basename "$f")&access_token=$GH_ACCESS_TOKEN"
# echo "asset = $GH_ASSET"

# curl --data-binary @"$f" -H "Content-Type: application/octet-stream" "$GH_ASSET"

# echo "Release successful!"
# exit 0
# ~/p/t/r/watr-works git:acs-dev ❯❯❯ scripts/release-textworks-binary.sh | jq '.'                                                                                                                                                                                                          ✱
# {
#   "name": "acs-dev",
#   "commit": {
#     "sha": "ef032cf9fc60c418fac736def2bdeadba4664039",
#     "node_id": "MDY6Q29tbWl0NDY0MjY1MDQ6ZWYwMzJjZjlmYzYwYzQxOGZhYzczNmRlZjJiZGVhZGJhNDY2NDAzOQ==",
#     "commit": {
#       "author": {
#         "name": "Adam Saunders",
#         "email": "saunders@cs.umass.edu",
#         "date": "2019-07-08T15:36:49Z"
#       },
#       "committer": {
#         "name": "Adam Saunders",
#         "email": "saunders@cs.umass.edu",
#         "date": "2019-07-08T15:36:49Z"
#       },
#       "message": "Setting version to 0.12-SNAPSHOT",
#       "tree": {
#         "sha": "994b4b35e0836d03c1a180b933da54e67d1f4757",
#         "url": "https://api.github.com/repos/iesl/watr-works/git/trees/994b4b35e0836d03c1a180b933da54e67d1f4757"
#       },
#       "url": "https://api.github.com/repos/iesl/watr-works/git/commits/ef032cf9fc60c418fac736def2bdeadba4664039",
#       "comment_count": 0,
#       "verification": {
#         "verified": false,
#         "reason": "unsigned",
#         "signature": null,
#         "payload": null
#       }
#     },
#     "url": "https://api.github.com/repos/iesl/watr-works/commits/ef032cf9fc60c418fac736def2bdeadba4664039",
#     "html_url": "https://github.com/iesl/watr-works/commit/ef032cf9fc60c418fac736def2bdeadba4664039",
#     "comments_url": "https://api.github.com/repos/iesl/watr-works/commits/ef032cf9fc60c418fac736def2bdeadba4664039/comments",
#     "author": {
#       "login": "adamchandra",
#       "id": 411952,
#       "node_id": "MDQ6VXNlcjQxMTk1Mg==",
#       "avatar_url": "https://avatars0.githubusercontent.com/u/411952?v=4",
#       "gravatar_id": "",
#       "url": "https://api.github.com/users/adamchandra",
#       "html_url": "https://github.com/adamchandra",
#       "followers_url": "https://api.github.com/users/adamchandra/followers",
#       "following_url": "https://api.github.com/users/adamchandra/following{/other_user}",
#       "gists_url": "https://api.github.com/users/adamchandra/gists{/gist_id}",
#       "starred_url": "https://api.github.com/users/adamchandra/starred{/owner}{/repo}",
#       "subscriptions_url": "https://api.github.com/users/adamchandra/subscriptions",
#       "organizations_url": "https://api.github.com/users/adamchandra/orgs",
#       "repos_url": "https://api.github.com/users/adamchandra/repos",
#       "events_url": "https://api.github.com/users/adamchandra/events{/privacy}",
#       "received_events_url": "https://api.github.com/users/adamchandra/received_events",
#       "type": "User",
#       "site_admin": false
#     },
#     "committer": {
#       "login": "adamchandra",
#       "id": 411952,
#       "node_id": "MDQ6VXNlcjQxMTk1Mg==",
#       "avatar_url": "https://avatars0.githubusercontent.com/u/411952?v=4",
#       "gravatar_id": "",
#       "url": "https://api.github.com/users/adamchandra",
#       "html_url": "https://github.com/adamchandra",
#       "followers_url": "https://api.github.com/users/adamchandra/followers",
#       "following_url": "https://api.github.com/users/adamchandra/following{/other_user}",
#       "gists_url": "https://api.github.com/users/adamchandra/gists{/gist_id}",
#       "starred_url": "https://api.github.com/users/adamchandra/starred{/owner}{/repo}",
#       "subscriptions_url": "https://api.github.com/users/adamchandra/subscriptions",
#       "organizations_url": "https://api.github.com/users/adamchandra/orgs",
#       "repos_url": "https://api.github.com/users/adamchandra/repos",
#       "events_url": "https://api.github.com/users/adamchandra/events{/privacy}",
#       "received_events_url": "https://api.github.com/users/adamchandra/received_events",
#       "type": "User",
#       "site_admin": false
#     },
#     "parents": [
#       {
#         "sha": "a9fe6094cebed68f81ef5cb3bc52e364752049f4",
#         "url": "https://api.github.com/repos/iesl/watr-works/commits/a9fe6094cebed68f81ef5cb3bc52e364752049f4",
#         "html_url": "https://github.com/iesl/watr-works/commit/a9fe6094cebed68f81ef5cb3bc52e364752049f4"
#       }
#     ]
#   },
#   "_links": {
#     "self": "https://api.github.com/repos/iesl/watr-works/branches/acs-dev",
#     "html": "https://github.com/iesl/watr-works/tree/acs-dev"
#   },
#   "protected": false,
#   "protection": {
#     "enabled": false,
#     "required_status_checks": {
#       "enforcement_level": "off",
#       "contexts": []
#     }
#   },
#   "protection_url": "https://api.github.com/repos/iesl/watr-works/branches/acs-dev/protection"
# }
