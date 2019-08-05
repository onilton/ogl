# Ogl

The prettiest git log graph in town!

![Demo](resources/demo.gif?raw=true "Title")

## Why

We all love the terminal. git log --graph is cool, but it can get really hard to read.

## Install

The easiest way is to get the single binary (available in releases page).

Install with a single command:

```
 sudo curl -o /usr/local/bin/ogl 'https://github.com/onilton/ogl/releases/download/v0.0.1/ogl' ; sudo chmod +x /usr/local/bin/ogl
```

Or with wget:

```
sudo wget 'https://github.com/onilton/ogl/releases/download/v0.0.1/ogl' -O /usr/local/bin/ogl ; sudo chmod +x /usr/local/bin/ogl
```
## Config file

You can change some settings like colors in `~/.ogl`. An example config:

```
####################                                                                                                                                                     
#Example Config
#################### 

# Seed is used to get a random color. We usually use the color from
# git log --graph itself but sometimes a color isn't provided so
# we need to choose a random color
#seed=3

# The graph/line style: "squared" "dual" "heavy" "default" (or "rounded")
style=rounded

# This fixes some column sizes. 
#unlimited-fields=false 
#subject.width=60
#author.width=15
#date.width=15

# Field colors
#subject.color=15
#author.color=66
#date.color=237

# Hide consecutive date and author
#hide-consecutive=true

# Align commit messages
#align-messages=false

# Number of columns to give up aligning commits
# When graph is too wide it can get hard to read commits
# even in pieces where it is not that wide yet
#max-align-messages=70

# Enable unicode icons for branch/ref names
#unicode-icons=true

# Personalize branch/ref icons
#icon.origin="🐱  "
#icon.tag="⚑ "
#icon.local="⌨  "
#icon.head="✓"

# Customize commit bullet 
#icon.commit.bullet="◦"
#icon.commit.childless="◦"
```

## TO-DO

* This was made as proof of concept, so the code needs a lot of love to become prettier
* More customizations
* Use other libs when they become available to scala-native
* Tests!!
* Packaging?
* It is fast. But if possible, make it (even) faster

# Questions

* Why scala? - Because I like it! :) To be honest I started with a prototype in python, but after facing some performance issues, and trying a little bit with cython, I decided to try scala-native.
* Wow, that scala code is ugly! - Unfortunately, idiomatic scala code is not that fast, and I tried to do that best possible to keep the time down. If you know how to improve the code without affecting performance, send a PR! :)
