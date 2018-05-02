# stack-network

[![Hackage](https://img.shields.io/hackage/v/lens.svg)](https://hackage.haskell.org/package/stack-network-0.1.0.1)

## About
This is a program I built as part of my Final Year Project in College. The project is titled __An Exploration into the Distribution of Stack__ in which I designed and tested a few approaches for distributing Stack. stack-network uses [Cloud Haskell](https://hackage.haskell.org/package/cloud-haskell) and [pipes](https://hackage.haskell.org/package/pipes) to efficiently transmit files between nodes on a network and thus minimise build times.

## Testing
 
__Prerequisites__

[Stack](https://docs.haskellstack.org/en/stable/README/)

[Docker](https://www.docker.com/) (installed and running)

`docker pull mcgizzle/stack-network`

`stack test`

This will run a number of scenarios created from `docker-compose` files embedded into the program using [Quasi-Quotes](https://wiki.haskell.org/Quasiquotation).

## Building & Running
 
`stack build`

Ensure you have a `network.config` file

```
net 
{
	host = "127.0.0.1"
	port = "5000"
}
```

Update the port and host as appropriate.

`stack exec stack-network --help` will list the options

To see the program in action it is suggested running it with Docker. I have provided an image that wil work with the tests and the docker-compose located in the project root.

Pull the image from Docker

`docker pull mcgizzle/stack-network`

Run this from the root of the project

`docker-compose up`



