#!/usr/bin/env perl

use v5.10;
use warnings;
use Time::HiRes qw(time);
use IPC::Open3;

my $type = lc(shift // '');
die 'type required: either arrays, fgl, fglc or generics'
	unless grep { $type eq $_ } qw(arrays fgl fglc generics);

sub timeit
{
	my $sub = shift;
	my $start = time;
	$sub->();
	return time - $start;
}


say sprintf "--- Compilation time: %.5fs", timeit sub {
	my $pid = open3 my $fh_in, '>&STDOUT', '>&STDERR', 'fpc', "-d$type", 'bench.pas';
	waitpid $pid, 0;
};

say sprintf "--- Run time: %.5fs", timeit sub {
	my $pid = open3 my $fh_in, '>&STDOUT', '>&STDERR', 'cat input.txt | ./bench';
	waitpid $pid, 0;
};

