#!/usr/bin/perl
use warnings;
use strict;

print "graph day12 {\n";

while (<>) {
    s/<->/--/;
    print;
}

print "}\n";

