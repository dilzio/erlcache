#!/bin/bash

erl +W w -pa ./out/lib/simple_cache/ebin/ -pa ./out/lib/resource_discovery/ebin/ -sname cache -boot ./out/lib/simple_cache -config ./res/sys -detached
