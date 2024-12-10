#!/usr/bin/perl

#
# Extract a line from a file that has a time specification similiar
# to cron, but, currently a limited version.
# Output the entry for the time.  I.e., if this were a crontab,
# then this would output the command.
#
# NOTE: THE LAST TIME MATCH IS OUTPUTTED!
#
# The format of each line is:
# MIN HOUR DAY MONTH YEAR PARAMETERS
#
# where:
# MIN      : 0 - 59
# HOUR     : 0 - 23
# DAY      : 1 - 31
# MONTH    : 1 - 12
# YEAR     : 1997...
# 
# You can specify:
#    *              --  Wild match.  This matches all possibilities.
#    n-m            --  Range.  n=begin, m=end, inclusive.
#    Exact values   --  Exact time specification.
#
# Example:
#
#       # at 6:10 a.m. every day
#       10 6 * * * date
#
#       # at 11:00-11:59 a.m. on the 4th for 1997 and 1998. 
#       * 11 4 * 1997-1998 date
#
#       # 4:00 a.m. on january 1st
#       0 4 1 1 * date
#
#       # once an hour.
#       0 * * * * date
#
#######################################################################
sub rangematch {
	local($item, $range) = @_;

	if ($range eq "*") { return 1; }
	$pos = index($range, "-");
	if ($pos > -1) { # Dealing w/ a range. 
		@range = split(/-/,$range);
		if ($range[0] <= $item && $item <= $range[1]) {
			return 1;
		} else {
			return 0;
		}
	} elsif ($item == $range) {
#		printf("<%s> range=<%s>\n", $item, $range);
		return 1;
	} else {
		return 0;
	}
}

sub datematch {
	local($a_month, $a_day, $a_year, $a_hour, $a_min,
		  $mon,     $day,   $year,   $hour,   $min) = @_;


#	printf("rangematch(%s, %s) is %d\n", $a_year, $year, &rangematch($a_year, $year));
#	printf("rangematch(%s, %s) is %d\n", $a_month, $mon, &rangematch($a_month, $mon));
#	printf("rangematch(%s, %s) is %d\n", $a_day, $day, &rangematch($a_day, $day));
#	printf("rangematch(%s, %s) is %d\n", $a_hour, $hour, &rangematch($a_hour, $hour));
#	printf("rangematch(%s, %s) is %d\n", $a_min, $min, &rangematch($a_min, $min));
	if (&rangematch($a_year, $year) == 1) { 
		if (&rangematch($a_month, $mon) == 1) { 
			if (&rangematch($a_day, $day)   == 1) {
				if (&rangematch($a_hour, $hour) == 1) {
					if (&rangematch($a_min, $min)   == 1) {
						return 1; }
				}
			}
		}
	}
# No match.
	return 0;
}

#
# Output, to stdout, the last entry that matches the time pattern.
#
# Usage:
#         grep_qc_parm.pl time
#
# Where 'time' is:
#     mm/dd/yy [hh[:mm]]

if ($#ARGV < 0 || $#ARGV > 1) {
	print STDERR "Usage: $0 mm/dd/yyyy [hh[:mm]]\n";
	exit(-1);
}

($a_month, $a_day, $a_year)  = split(/\//, $ARGV[0]); shift;
$a_year += 1900 if $a_year < 70;
$a_year += 1900 if $a_year >= 70 && $a_year < 100;
($a_hour,  $a_min)   = split(/:/, $ARGV[0]) if ($#ARGV == 0); shift;

while (<>) {
	split;
	next if /^#/; # Skip comments.
	next if $#_ < 5;
	($min, $hour, $day, $month, $year) = @_;
# Save the match; overwrite any previous time match.
	$parameters = join(' ', @_[5..$#_])
		if &datematch($a_month, $a_day, $a_year, $a_hour, $a_min,
					   $month,   $day,   $year,   $hour,   $min);
}

# What was the last match?
print $parameters, "\n";
