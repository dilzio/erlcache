OUTDIR=./out/lib 
SC_OUT_DIR=$OUTDIR/simple_cache/ebin 
RD_OUT_DIR=$OUTDIR/resource_discovery/ebin 

rm -rf $OUTDIR
mkdir -p $SC_OUT_DIR
mkdir -p $RD_OUT_DIR


erlc +debug_info -o $SC_OUT_DIR ./src/simple_cache/*.erl
cp ./res/simple_cache.app $SC_OUT_DIR

erlc +debug_info -o $RD_OUT_DIR ./src/resource_discovery/*.erl
cp ./res/resource_discovery.app $RD_OUT_DIR

dialyzer -I ./out/lib/simple_cache/ebin/*.beam ./out/lib/resource_discovery/ebin
