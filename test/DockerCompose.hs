{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module DockerCompose where

import           Data.ByteString   (ByteString)
import           Text.RawString.QQ

fourNYaml :: ByteString
fourNYaml =
  [r|
slave3:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7003|' network.config && sed -i -e 's|mwc-random|mtl,transformers|' testbuild1.cabal && stack-network join"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7003:7003"
slave2:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7002|' network.config && sed -i -e 's|mwc-random|transformers|' testbuild1.cabal && stack-network join"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7002:7002"
slave1:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7001|' network.config && sed -i -e 's|mwc-random|mtl,mwc-random|' testbuild1.cabal && stack-network join"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7001:7001"
master:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7000|' network.config && sed -i -e 's|mwc-random|mtl,mwc-random|' testbuild1.cabal && stack-network build -n 3"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7000:7000"
|]

threeNYaml :: ByteString
threeNYaml =
  [r|
slave2:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7002|' network.config && sed -i -e 's|mwc-random|primitive|' testbuild1.cabal && stack-network join"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7002:7002"
slave1:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7001|' network.config && sed -i -e 's|mwc-random|vector|' testbuild1.cabal && stack-network join"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7001:7001"
master:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7000|' network.config && sed -i -e 's|mwc-random|primitive|' testbuild1.cabal && stack-network build -n 2"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7000:7000"
|]

simpleYaml :: ByteString
simpleYaml =
  [r|
slave:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7001|' network.config && sed -i -e 's|mwc-random|array|' testbuild1.cabal && stack-network join"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7001:7001"
master:
  image: mcgizzle/stack-network
  command: bash -c "cd testbuild && sed -i -e 's|&port|7000|' network.config && sed -i -e 's|mwc-random|array|' testbuild1.cabal && stack-network build"
  tty: true
  stdin_open: true
  net: "host"
  ports:
    - "7000:7000"
|]
