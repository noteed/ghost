#! /bin/sh
# Run this script from user with Ghost administrator role.
# It adds the user with its key to Ghost.

username=$1
public_key=$2

scp $public_key ghost@localhost:delete-me.txt
ssh ghost@localhost bin/ghost-control add-user -u $username -k delete-me.txt
ssh ghost@localhost rm delete-me.txt
