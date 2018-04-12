#!/bin/bash
echo "Building image.."
docker build -t $1 -f Dockerfile.base . &&
echo "Building Stack..."
stack --docker build &&
echo "Creating image container..."
stack --docker image container &&
docker-compose up
echo "Done diddly done :)"
