#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 9;

test_output("Macro - no args, no return", q{
	define foo() { print(42); }
	install { foo(); }
}, q{
	OUTV 42
});

test_output("Macro - args, no return", q{
	define foo(numeric x) { print(x); }
	install { foo(42); }
}, q{OUTV 42});

test_output("Macro - return, no args", q{
	define foo() returning numeric { return = 42; }
	install { print(foo()); }
}, q{OUTV 42});

test_output("Macro - return, args", q{
	define foo(numeric x) returning numeric { return = x; }
	install { print(foo(42)); }
}, q{OUTV 42});

test_output("Macros can call macros", q{
	define foo(numeric x) { print(x); }
	define bar(numeric x) { foo(x); }
	install { bar(42); }
}, q{OUTV 42});

test_error("Macros cannot call later-defined macros", q{
	define foo(numeric x) { bar(x); }
	define bar(numeric x) { print(x); }
	install { foo(42); }
}, "unknown macro");

test_error("Macros cannot be redefined automatically", q{
	define foo(numeric x) { }
	define foo(numeric y) { }
}, "redefine");

test_output("Macros with differing numbers of arguments can be overloaded", q{
	define foo(numeric x) { }
	define foo(numeric x, numeric y) { }
}, q{});

test_output("Macros can be forcibly redefined", q{
	define foo(numeric x) { }
	redefine foo(numeric x) { }
}, q{});
