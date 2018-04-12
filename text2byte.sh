#!/bin/bash

if [ $1 = "Text" ]
then
  other="ByteString"
else
  other="Text"
fi

sed -i '' "s/($1,/($other,/" src/Network/Distributed/Types.hs

echo "Done"
