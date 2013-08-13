OUTDIR=./out/lib 
SC_OUT_DIR=$OUTDIR/simple_cache/ebin 
RD_OUT_DIR=$OUTDIR/resource_discovery/ebin 
#clean
rm -rf $OUTDIR
mkdir -p $SC_OUT_DIR
mkdir -p $RD_OUT_DIR

#compile
erlc +debug_info -o $SC_OUT_DIR ./src/simple_cache/*.erl
cp ./res/simple_cache.app $SC_OUT_DIR

erlc +debug_info -o $RD_OUT_DIR ./src/resource_discovery/*.erl
cp ./res/resource_discovery.app $RD_OUT_DIR

#dialyze
dialyzer -I ./out/lib/simple_cache/ebin/*.beam ./out/lib/resource_discovery/ebin

#generate boot scripts
erl -detached -pa ./res -eval 'systools:make_script("simple_cache", [local, {outdir, "./out/lib"}]), init:stop().'
