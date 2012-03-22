#! /bin/sh
sudo userdel ghost ; sudo rm -rf /home/ghost
./create-ghost-user.archlinux.sh ~/.ssh/id_rsa.pub
./add-user.sh noteed ~/.ssh/rum_key.pub
./add-repository.sh noteed ghost
