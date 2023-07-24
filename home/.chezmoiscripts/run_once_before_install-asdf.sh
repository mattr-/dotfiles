#!/bin/bash
if [ ! -e $HOME/.asdf ]
then
  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.12.0
fi
