#!/bin/sh

scp -r watr-color-server/target/universal/stage/ vinci8:/m/vinci8/data1/saunders/bioarxiv-watrcolors-root/scp-inbox/watrcolors-dist
scp -r watr-shed/target/universal/stage/ vinci8:/m/vinci8/data1/saunders/bioarxiv-watrcolors-root/scp-inbox/watrshed-dist
scp -r watr-jslibs/dist vinci8:/m/vinci8/data1/saunders/bioarxiv-watrcolors-root/scp-inbox/watr-jslib-dist
