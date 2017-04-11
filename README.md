# Goodreads Reading List

A unified inbox for Asana.

## Development

This project is a single-page application written in [Elm](http://elm-lang.org/) and a node.js API using Express.

You need to start by installing the following base dependencies:

+ node
+ elm
+ docker

Then install all dependencies:

    yarn install
    yarn bootstrap

### Start development environment

    yarn start-dev

You can also just start the client or server individually

    yarn start-server-dev
    yarn start-client-dev

### Deploying to production

    yarn deploy
