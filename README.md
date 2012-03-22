# Ghost -- Git hosting and deployment

(Nothing from the following text is true yet, but might be some day. Actually I
write some stuffs that are useless on their own but let me understand/practice
the concepts.)

Ghost is a Git repositories hosting suite written in Haskell (released under a
BSD3 license). Other such tools include Gitosis, Gitolite, and Gitorious.

## Installation

Installing Ghost on the server involves creating a ghost user and a few files
and directories. On Arch Linux, this is conveniently done by running the
provided `create-ghost-user.archlinux.sh` script from a regular user account.
The script uses `sudo` so the user should have the appropriate rights.
