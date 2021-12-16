#!/bin/sh

day=$1
source .env
mkdir -p inputs
curl "https://adventofcode.com/2021/day/$day/input" -H 'accept-encoding: gzip, deflate, br' -H "cookie: session=$SESSION" --compressed > inputs/day$day.txt
