# Asana Unified Inbox

A proof of concept of an unified inbox for Asana. Uses access tokens instead of oAuth so it works across workspaces and different users. The access token is currently stored in localStorage in your browser.

You can create Asana access tokens in 'My Profile Settings' > Apps > Manage Developer Apps.

A huge work in progress so take care. There are bugs.

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
