#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 2;

test_output("AND (unfolded)", q{
	numeric x = __touch(1);
	print(x & 2);
}, q{
	SETV $1 1
	ANDV $1 2
	OUTV $1
	RSCR
});

test_output("AND (folded)", q{
	numeric x = 3;
	print(x & 6);
}, q{
	OUTV 2
	RSCR
});
