#!/bin/bash

# Fix permissions for shared cabal store
sudo chown vscode /home/vscode/.cabal/store

# Install system dependencies
sudo apt-get update
sudo apt-get install -y clang g++
