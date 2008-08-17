#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 3;

test_output("Constant expressions in script block", q[
	define numeric tx = 42;
	define numeric ty = 24;
	define tz() returning numeric { return = 123; }
	script (tx, ty, tx+ty, tz) { }
], q{
	SCRP 42 24 66 123
	ENDM
});

test_error("Reject non-constant expression in script block", q[
	define tx() returning numeric { return = norn.x; }
	script (tx, 1, 1, 1) { }
], "constant value expected");

test_error("Reject side-effecting expression in script block", q[
	define tx() returning numeric { print(42); return = 42; }
	script(tx, 1, 1, 1) { }
], "side-effects not allowed");
