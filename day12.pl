#!/usr/bin/perl
use warnings;
use strict;

print "graph day12 {\n";

while (<>) {
    s/<->/--/;
    s/,/ -- /g;
    print $_;
}

print "}\n";

