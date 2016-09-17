#!perl -w
use strict;
use File::MimeInfo::SharedMimeInfoXML;
my $mime = File::MimeInfo::SharedMimeInfoXML->new(
    database => ['t/freedesktop.org.xml'],
);

if( $^O =~ /mswin/i ) {
    require File::Glob;
    @ARGV = grep { ! -d } map { File::Glob::bsd_glob $_ } @ARGV;
};

for my $file (@ARGV) {
    my $t = $mime->mimetype($file);
    $t = $t ? $t->mime_type : "<unknown>";
    print sprintf "%s: %s\n", $file, $t;
}
