#!/bin/sh
R CMD BATCH ./01-lookup-to-reduce.R
R CMD BATCH ./02-lookup-to-finalize.R
echo 'done'
