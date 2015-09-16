#!/bin/bash
ghcid --test="$(echo "import Test.DocTest"; echo 'doctest ["-isrc", "src/Tests.hs"]')"
