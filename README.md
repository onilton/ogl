# Ogl

The prettiest git log graph in town!

## Why

We all love the terminal. git log --graph is cool, but it can get really hard to read.

See it in action:


## Install

The easiest way is to get the single binary (available in releases page).

Install with a single command:

```
sudo wget 'https://github.com/onilton/ogl/releases/download/v0.0.1/ogl' -O /usr/local/bin/ogl
```

## Features

## Config file

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
