# stack-network

## About
This is a program I built as part of my Final Year Project in College. The project is titled __An Exploration into the Distribution of Stack__ in which I designed a tested a few approaches for distributing Stack. stack-network uses Cloud Haskell to transmit files between nodes on a network and thus minimise build times.

## Testing
 
__Prerequisites__

[Stack](https://docs.haskellstack.org/en/stable/README/)

[Docker](https://www.docker.com/) (installed and running)

`docker pull mcgizzle/stack-network`

`stack test`

Be sure to run this from the root of the project.

This will run the [docker-compose.yml](https://github.com/McGizzle/stack-network/blob/master/test/docker-compose.yml) located in the test folder.

## Building & Running
 
`stack build`

Ensure you have a `network.config` file matching the one in the repo. Update the port and host as appropriate.

`stack exec stack-network [join | build]`

To see the program in action it is suggested running it with Docker. 

`docker pull mcgizzle/stack-network`

Run this from the root of the project

`docker-compose up`

Alternatively you can build the image from the provided [Dockerfile](https://github.com/McGizzle/stack-network/blob/master/Dockerfile.base)

This provides greater control (through editing the file) built will take some time to build. It is recommneded using the provided image

Just run the following script

`sh rundock.sh stack-net-base` 



