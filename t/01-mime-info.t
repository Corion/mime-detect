#!perl -w
use strict;
use Test::More tests => 9;
use File::MimeInfo::SharedMimeInfoXML;
my $mime = File::MimeInfo::SharedMimeInfoXML->new();
$mime->read_database('t/freedesktop.org.xml');

my $pgp = $mime->known_types->{'application/pgp-signature'};

ok $pgp, "We find a type for 'application/pgp-signature'";
my $superclass = $pgp->superclass;
if( !ok $superclass, "We have a superclass") {
    use Data::Dumper;
    diag Dumper $pgp;
    SKIP: { skip "We didn't even find a superclass", 1 };
} else {
    is $pgp->superclass->mime_type, 'text/plain', "It's a text file";
    
    ok $pgp->matches(<<'PGP'), "We match some fake PGP file";
-----BEGIN PGP SIGNATURE-----
some random gibberish
qweoibvsjewrij
PGP
};

my $perl = $mime->known_types->{'application/x-perl'};

ok $perl, "We find a type for 'application/x-perl'";
my $superclass = $perl->superclass;
if( !ok $superclass, "We have a superclass") {
    use Data::Dumper;
    diag Dumper $perl;
    SKIP: { skip "We didn't even find a superclass", 1 };
} else {
    is $perl->superclass->mime_type, 'application/x-executable', "It's an executable file";
    
    ok $perl->matches(<<'PERL'), "We match some fake PERL file";
#!perl -w
use strict;
some random gibberish
qweoibvsjewrij
PERL
};

my $sevenZip = $mime->known_types->{'application/x-7z-compressed'};
ok $sevenZip->matches("7z\274\257'\34\0"), "We identify 7zip files correctly";
