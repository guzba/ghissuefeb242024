import ../src/ghissuefeb242024, std/times

# nim c --mm:arc --threads:on -d:release --debugger:native -rf .\tests\bench.nim
# nim c --mm:arc --threads:off -d:release --debugger:native -rf .\tests\bench.nim

let twitterJson = readFile("tests/data/twitter.json")

let start = epochTime()
for _ in 0 ..< 1000:
  discard parseJson(twitterJson)
echo epochTime() - start

# --threads:on takes ~1.6s on my machine
# --threads:off takes ~0.9s on my machine

# Nim Compiler Version 2.0.2 [Windows: amd64]
# Compiled at 2023-12-15
# Copyright (c) 2006-2023 by Andreas Rumpf

# gcc version 11.1.0 (MinGW-W64 x86_64-posix-seh, built by Brecht Sanders)
