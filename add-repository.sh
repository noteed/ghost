#! /bin/sh
# Run this script from user with Ghost administrator role.
# It adds a repository to a user under Ghost..

username=$1
repository=$2

ssh ghost@localhost bin/ghost-control add-repository -u $username --repo $repository
