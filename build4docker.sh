#!/bin/bash

function myReplace {
if [ "$1" = "Text" ]
then
  other="ByteString"
else
  other="Text"
fi

sed -i '' "s/($1,/($other,/" src/Network/Distributed/Types.hs

echo "Swapped $1 for $other"
}

myReplace "Text" && 
stack --docker build && 
stack --docker image container && 
myReplace "ByteString" &&
stack test

