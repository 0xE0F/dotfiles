#!/bin/sh
############################

TARGET_DIR=~/dotfiles
BACKUP_DIR=~/dotfiles_backup
SOURCES="gitconfig bashrc vimrc zshrc oh-my-zsh"

echo "Creating backup directory in $BACKUP_DIR"
mkdir -p $BACKUP_DIR

cd $TARGET_DIR

for file in $SOURCES; do
    echo "Moving any existing dotfiles from ~ to $BACKUP_DIR"
    mv ~/.$file ~/${BACKUP_DIR}/
    echo "Creating symlink to $file in home directory."
    ln -s $TARGET_DIR/$file ~/.$file
done
