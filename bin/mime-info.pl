#!perl -w
use strict;
use File::MimeInfo::SharedMimeInfoXML;
my $mime = File::MimeInfo::SharedMimeInfoXML->new();
$mime->read_file('t/freedesktop.org.xml');

if( $^O =~ /mswin/i ) {
    require File::Glob;
    @ARGV = grep { ! -d } map { File::Glob::bsd_glob $_ } @ARGV;
};

for my $file (@ARGV) {
    print sprintf "%s: %s\n", $file, $_->mime_type
        for $mime->mimetype($file);
}
