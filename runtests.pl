#!/usr/bin/perl

use strict;
use warnings;

use Test::Harness;
use POSIX qw(getcwd);

exec("prove", "-I".getcwd()."/t/lib", "t", @ARGV);
