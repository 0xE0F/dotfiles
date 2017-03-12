#!/bin/sh
############################

TARGET_DIR=$(pwd)
BACKUP_DIR=$(pwd)/dotfiles_backup
SOURCES="gitconfig vimrc zshrc"

echo "Creating backup directory in $BACKUP_DIR"
mkdir -p $BACKUP_DIR

cd $TARGET_DIR

for file in $SOURCES; do
    echo "Moving any existing dotfiles from ~ to $BACKUP_DIR"
    mv ~/.$file ${BACKUP_DIR}/
    echo "Creating symlink to $file in home directory."
    ln -s $TARGET_DIR/$file ~/.$file
done
