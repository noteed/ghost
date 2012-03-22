#! /bin/sh
# Run this script as a regular user, with your public SSH  key as argument.
# Ensure before running the script a SSH server is running and will accept
# connections for the ghost user.
#
# This creates a ghost user, a few directories in its home, copy the SSH
# key as authorized_keys then ssh into localhost as the ghost user to
# call ghost-control init which completes the setup.

# TODO:
# - test $1 is given and is a public key
# - test ghost-control exists somewhere

public_key=$1

sudo useradd \
  --system \
  --shell /bin/sh \
  --create-home \
  --user-group \
  ghost

sudo mkdir -p /home/ghost/{.ssh,bin,administrator}
sudo cp `which ghost-command` /home/ghost/bin/
sudo cp `which ghost-control` /home/ghost/bin/
sudo cp $public_key /home/ghost/.ssh/authorized_keys
sudo chown -R ghost:ghost /home/ghost/
ssh ghost@localhost bin/ghost-control init
