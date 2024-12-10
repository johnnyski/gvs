#!/usr/bin/perl
$nargs = 1-1;
if ($#ARGV < $nargs) {
        print STDERR "Usage: $0 directory\n";
        exit;
}

$dir = shift;
chdir $dir;		# Change directory to $dir
print "Processing data from $dir\n";

$prog  = "2A53";
$zr    = "/usr/people/trmm/win/zr/zr_mlb_test.out";
$gauge = "/dat4/trmm/AIW/gauge/melbourne/mel_sitelist95";
#$win   = "gwin/gwin.out";
$win   = "gwin.out";

$options = " -b 1.5 -f 4.0 -n 0 -p 1000 -t 303 -d 295 -z $zr -g $gauge -w $win -k 300. -a 1.4 -m 0 -c";

$ls_command = "ls uf/ | "; # Pipe the uf filenames into ls_command

open(LIST,$ls_command); # This creates a list of all of the filenames in the
			# uf directory. We will create the c/s and output
			# filenames from the date/time of the UF file.

while(<LIST>) {		# For each filename in LIST, process...
	chop;
	$uf_file  = "uf/".$_;			# UF file
	$uf_date  = substr($uf_file,7,6);	# Date from UF filename
	$uf_time  = substr($uf_file,14,4);	# Time from UF filename

	$cs_file  = "cs/csmap.ascii.".$uf_date.".".$uf_time; 	# C/S filename
#	$out_file = "rain/rain_".$uf_date."_".$uf_time.".dat";	# Output
	$out_file = "rain.dat";	# Output

	$command = "$prog $options $cs_file $uf_file $out_file";

	print "$command\n";	# You can comment this line out with # sign
	system($command);	# This runs the program for the given files
}
