#!/bin/bash
erl +W w -pa ./out/lib/simple_cache/ebin/ -pa ./out/lib/resource_discovery/ebin/ -pa ./out/lib/tcp_interface/ebin/ -pa ./out/lib http_interface/ebin -pa ./out/lib/gen_web_server/ebin -sname cache@localhost
