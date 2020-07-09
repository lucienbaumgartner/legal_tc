#!/bin/sh
echo $1
find -E ./../../output/01-reduced-corpora/baseline/reddit -regex $1 -delete
find -E ./../../output/02-finalized-corpora/baseline/reddit -regex $1 -delete
find -E ./../../output/00-bulk-data/baseline/reddit/raw -regex $1 -delete
find -E ./../../output/00-bulk-data/baseline/reddit/raw_aggr -regex $1 -delete
echo 'done'
