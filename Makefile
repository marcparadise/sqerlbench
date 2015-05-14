all: compile bench gp_throughput gp_latencies graph

deps/:
	rebar get-deps -j 4

compile: basho_bench/basho_bench deps/
	rebar compile -j 4

basho_bench/:
	git clone git://github.com/basho/basho_bench.git

bench: compile
	basho_bench/basho_bench --results-dir results ./priv/tests.config

nbench: compile
	basho_bench/basho_bench -n $(NAME) --results-dir results priv/tests.config

clean:
	rm -rf deps
	rm -rf ebin

allclean: clean
	rm -rf results

basho_bench/basho_bench:
	$(MAKE) -C basho_bench -j 4

# Targets for graph generation

# Not working yet
gt:
	gnuplot priv/current.gp

# Not working yet
gta:
	gnuplot priv/all.gp

# Is working
gp_throughput:
	basho_bench/priv/gp_throughput.sh -d results/current

# Is working
gp_latencies:
	basho_bench/priv/gp_latencies.sh -d results/current

# Is working
graph:
	basho_bench/priv/summary.r -i results/current

