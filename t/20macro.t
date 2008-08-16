#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 1;

test_output("Constant expressions in script block", q[
	define numeric tx = 42;
	define numeric ty = 24;
	define tz() returning numeric { return = 123; }
	script (tx, ty, tx+ty, tz) { }
], q{
	SCRP 42 24 66 123
	ENDM
});

__END__

## Can't test for error yet
test_error("Reject non-constant expression in script block", q[
	define numeric tx() { $return = norn.x; }
	script (tx, 1, 1, 1) { }
]);
