# AIwaffle Website

A visual tutorial of machine learning.

# How to add tutorial contents
1. Write a tutorial in markdown.
2. Submit a pull request to merge into dist/contents folder

See [sample tutorial content](https://github.com/AlienKevin/AIwaffle-website/blob/master/dist/contents/Introduction.md).

# Development
1. Install elm-live using `npm install elm-live`
2. run
```
elm-live src/Main.elm --pushstate --start-page dist/index.html -- --output=dist/main.js
```
3. In your browser, go to address `localhost:8000`

# Deployment
Run `./build.sh`. You may need to enable execution permission.

# License
MIT

# Change Log

Release v0.3.0
* Added 2 Jupyter notebook tutorials according to [AIWaffle's course list](https://github.com/AIwaffle/AIwaffle/blob/master/Courses/course-list.md)

Release v0.2.0
* Add About Page ([#1](https://github.com/AIwaffle/AIwaffle-website/issues/1))
* Add 2 Jupyter notebook tutorials ([#2](https://github.com/AIwaffle/AIwaffle-website/issues/2))
