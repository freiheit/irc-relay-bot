#!/bin/sh
nohup sh -c "while sleep 10 ; do ./relay-bot.pl >> relay-bot.log 2>&1 ; done" &
