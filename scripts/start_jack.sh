#!/bin/bash

jackd -d alsa -d hw:1,0 -r48000 -p256 -n3
