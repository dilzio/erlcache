#!/bin/bash
OUTDIR=./out/lib 
SC_OUT_DIR=$OUTDIR/simple_cache/ebin 
RD_OUT_DIR=$OUTDIR/resource_discovery/ebin 
TI_OUT_DIR=$OUTDIR/tcp_interface/ebin 
GWS_OUT_DIR=$OUTDIR/gen_web_server/ebin 
#clean
rm -rf $OUTDIR
mkdir -p $SC_OUT_DIR
mkdir -p $RD_OUT_DIR
mkdir -p $TI_OUT_DIR
mkdir -p $GWS_OUT_DIR

#compile
erlc +debug_info -o $SC_OUT_DIR ./src/simple_cache/*.erl
cp ./res/simple_cache.app $SC_OUT_DIR

erlc +debug_info -o $RD_OUT_DIR ./src/resource_discovery/*.erl
cp ./res/resource_discovery.app $RD_OUT_DIR

erlc +debug_info -o $TI_OUT_DIR ./src/tcp_interface/*.erl
cp ./res/tcp_interface.app $TI_OUT_DIR

erlc +debug_info -o $GWS_OUT_DIR ./src/gen_web_server/*.erl
cp ./res/tcp_interface.app $TI_OUT_DIR

#dialyze
dialyzer -I ./out/lib/simple_cache/ebin/*.beam ./out/lib/resource_discovery/ebin

#generate boot scripts
erl -detached -pa ./res -eval 'systools:make_script("simple_cache", [local, {outdir, "./out/lib"}]), init:stop().'

#copy .rel & config files
cp ./res/simple_cache.rel $OUTDIR
cp ./res/sys.config $OUTDIR

#make tarball
cd $OUTDIR
erl -detached -pa ./simple_cache/ebin -pa ./resource_discovery/ebin -eval 'systools:make_tar("simple_cache", [{erts, code:root_dir()}]), init:stop().'
