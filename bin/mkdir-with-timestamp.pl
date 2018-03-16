#!/usr/bin/perl -w

my $prefix = shift;
defined( $prefix ) or print( "usage: mkdir-timestamp dirname\n" ) and exit();

chomp $prefix;

my $DATE=`date`;
chomp $DATE;
$DATE =~ s/[ ]+/-/g;
$DATE =~ s/:/-/g;
my $FILENAME = "$prefix"."-$DATE";

`mkdir $FILENAME`;

print "$FILENAME\n"


