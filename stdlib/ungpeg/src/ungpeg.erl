-module(ungpeg).
-compile(export_all).

build() ->
  neotoma:file("priv/ungpeg_n.peg"),
  compile:file("priv/ungpeg_n.erl",             [{outdir, "./ebin"}, report]),
  compile:file("src/ungpeg_optimize.erl",       [{outdir, "./ebin"}, report]),
  compile:file("src/ungpeg_write_compiler.erl", [{outdir, "./ebin"}, report]),
  compile:file("src/ungpeg.erl",                [{outdir, "./ebin"}, report]).
