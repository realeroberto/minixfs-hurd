## function-tailoring.sh: called by make on `.c.in' files.
##
## The most significant difference between the two versions of the
## MINIX file system lies in the width of the data type each version
## uses to represent the address of a zone: either 16-bits (V1) or
## 32-bits (V2) integers.
##
## Accordingly, one should either have written much code twice, with
## the only change of some variable declarations, or have used a
## conditional around almost every single operation involving zone
## addresses.  An elegant implementation, which avoids the drawbacks
## of both these methods, is to be found in the Linux kernel.
##
## For the sake of variety, I resolved to resort to a solution which
## is outwardly different from that by Linus (the substance being much
## the same, I believe).
##
## This script is used to avoid duplication of code.  It accepts a
## text file as input, and produces an output file according to the
## rules described below.
##
## A region of text between the markers
## __BEGIN_FS_VERSION_SPECIFIC_CODE and __END_FS_VERSION_SPECIFIC_CODE
## is copied twice, with the following changes taking place:
##
##  * all occurrences of FS_VERSION are replaced with the strings `V1'
##  (the first time) and `V2' (the second time);
##
##  * all occurrences of ZONE_ADDRESS_T are replaced with the qualifier
##  of a data type suitable to represent zone addresses in the given
##  file system's version: namely, `uint16_t' (the first time) and
##  `uint32_t' (the second time).
##
## The markers themselves are discarded.
##
## The remainder of the input is copied verbatim.
##
## A small heading is prepended which intimates that the output has
## been automatically generated and should not be changed directly.
## Also, CPP's `#line' directives are inserted for the programmer's
## convenience.

section_begin=__BEGIN_FS_VERSION_SPECIFIC_CODE
section_end=__END_FS_VERSION_SPECIFIC_CODE

scriptname=`basename $0`
today=`date -u`
force=
infile=
outfile=

usage ()
{
    cat <<EOF
Usage: $0 [-h] [-o OUTFILE] INFILE

-f	overwrite existing OUTFILE
-h	print this message
-o      place output in file OUTFILE
EOF
}

while :
do
  case "$1" in
      -f) force=y ;;
      -h) usage ; exit 1 ;;
      -o) outfile=$2 ; shift ;;
      *) break ;;
  esac
  shift
done

if [ -z "$1" ]; then
    usage ; exit 1
else
    infile=$1
    if [ -n "$2" ]; then
	usage ; exit 1
    fi
fi

if [ ! -s "$infile" ]; then
    echo $0: input file \`$infile\' does not exist or it is empty.
    exit 1
elif [ ! -r "$infile" ]; then
    echo $0: cannot read input file \`$infile\'.
    exit 1
fi

if [ -z "$outfile" ]; then
    outfile=`echo $infile | sed 's/\.in$//'`
fi

if [ "$infile" == "$outfile" ]; then
    echo $0: output file \`$outfile\' cannot be the same as input file.
    echo Please either specify a different output file name, or rename your
    echo input file so as to give it a \`.c.in\' extension.
    exit 1
fi

echo $outfile | grep '\.c$' > /dev/null || {
    echo $0: warning: output file \`$outfile\' 
}

# As we want to take advantage of cpp's `#line' directives, we need
# to keep track of line numbers in the input file.
cat -n $infile | sed -n "

1 {
    i\\
/* $outfile - automatically generated from $infile */\\
/* by $scriptname on $today */\\
\\
#line 1 \"$infile\"
}

s/^ *\([0-9]*\)	$section_begin/#line \1 \"$infile\"/; t loop
s/^ *[0-9]*	//p
b

:loop
s/$section_end//; t end
N
b loop

:end
s/ *[0-9]*	//g
# save pattern space, since we're about to change its contents
h

s/FS_VERSION/V1/g
s/ZONE_ADDRESS_T/uint16_t/g
p

g
s/FS_VERSION/V2/g
s/ZONE_ADDRESS_T/uint32_t/g
p

b" > $outfile
