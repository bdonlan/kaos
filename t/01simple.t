#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 1;

test_output("Constant propagation", qq{
	numeric x = 42;
	print(x);
}, qq{
	OUTV 42
	RSCR
});
