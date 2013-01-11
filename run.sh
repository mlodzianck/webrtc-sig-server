#! /bin/sh
erl -pa ebin/ deps/edown/ebin/ deps/gen_leader/ebin/ deps/gproc/ebin/ deps/ibrowse/ebin/ deps/yaws/ebin/ -gproc gproc_dist all
