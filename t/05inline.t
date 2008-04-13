#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 6;

test_output("Inline - same scope", q{
	numeric foo;
	_caos { .inline let $foo = FOO; }
	print(foo);
}, q{
	OUTV FOO
});

test_output("Inline - different scope, within statement", q{
	define foo() returning numeric {
		_caos { .inline let $return = FOO; }
	}
	install {
		print(foo());
	}
}, q{
	OUTV FOO
});

test_output("Inline - different scope, cross statement", q{
	define foo() returning numeric {
		_caos { .inline let $return = FOO; }
	}
	install {
		numeric bar = foo();
		print(bar);
	}
}, q{
	SETV $1 FOO
	OUTV $1
});

test_output("Static - same scope", q{
	numeric foo;
	_caos { .static let $foo = FOO; }
	print(foo);
}, q{
	OUTV FOO
});

test_output("Static - different scope, within statement", q{
	define foo() returning numeric {
		_caos { .static let $return = FOO; }
	}
	install {
		print(foo());
	}
}, q{
	OUTV FOO
});

test_output("Static - different scope, cross statement", q{
	define foo() returning numeric {
		_caos { .static let $return = FOO; }
	}
	install {
		numeric bar = foo();
		print(bar);
	}
}, q{
	OUTV FOO
});
