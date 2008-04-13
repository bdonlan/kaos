#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 1;

test_output("anim", q{
	null.anim(1,2,3,4);
}, q{
	TARG NULL
	ANIM [ 1 2 3 4 ]
	RSCR
});
