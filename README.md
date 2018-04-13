# stack-network

## About
This is a program I built as part of my Final Year Project in College. The project is titled __An Exploration into the Distribution of Stack__ in which I designed a tested a few approaches for distributing Stack. stack-network uses Cloud Haskell to transmit files between nodes on a network and thus minimis build times.

## Testing

__Prerequisites__

[Stack]()

[Docker]() is installed and running

`stack test`

This will run the docker-compose.yml located in the test folder.

## Building & Running

`stack build`

Ensure you have a `network.config` file matching the one in the repo. Update the port and host as appropriate.

`stack exec stack-network [join | build]`

To see the program in action it is suggested running it with Docker. 

To create the image, build for docker and run it use the shell script provided.

`sh rundock.sh [image name]` 



