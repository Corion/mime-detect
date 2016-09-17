#!perl -w
use strict;
use Test::More tests => 3;
use File::MimeInfo::SharedMimeInfoXML;
use Data::Dumper;

my $mime = File::MimeInfo::SharedMimeInfoXML->new(
    database => ['t/freedesktop.org.xml','t/custom.xml'],
);

open my $fh, '<', \"7z\274\257'\34\0"
    or die "Couldn't open in-memory 7z file: $!";
my @types = $mime->mime_types($fh);

if( !ok 0+@types, "We identify our buffer with at least one type" ) {
    SKIP: { skip "Didn't identify our buffer", 1 };
} else {
    is $types[0]->mime_type, "application/x-7z-custom", "We recognize a custom type";
    is_deeply [map {;$_->mime_type} @types], [
        "application/x-7z-custom",
        "application/x-7z-compressed",
    ], "We still recognize all types";
};

done_testing;