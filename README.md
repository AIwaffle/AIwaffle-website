# AIwaffle Website

A visual tutorial of machine learning.

# How to add tutorial contents
1. Write a tutorial in markdown.
2. Submit a pull request to merge into dist/contents folder

See [sample tutorial content](https://github.com/AlienKevin/AIwaffle-website/blob/master/dist/contents/Introduction.md).

# Build locally
1. Install elm-live using `npm install elm-live`
2. run `elm-live src/Main.elm --pushstate --start-page dist/index.html --host 0.0.0.0 -- --output=dist/main.js`. The `--host 0.0.0.0` is only required for Windows Subsystem for Linux 2.
3. In your browser, go to address `localhost:8000`

# License
MIT
