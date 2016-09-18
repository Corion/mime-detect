package File::MimeInfo::SharedMimeInfoXML::FreedesktopOrgDB;
use strict;
use vars '$VERSION';
$VERSION = '0.01';

=head1 NAME

File::MimeInfo::SharedMimeInfoXML::FreedesktopOrgDB - default freedesktop.org database

=head1 NOTICE

This distribution contains a verbatim copy of the freedesktop.org
MIME database available from
L<https://www.freedesktop.org/wiki/Software/shared-mime-info/>
.
That database is licensed under the General Public License v2,
see the accompanying COPYING file distributed with the file
for its exact terms.

=cut

sub url {'https://www.freedesktop.org/wiki/Software/shared-mime-info/'}

=head2 C<< get_xml >>

    my $xml = File::MimeInfo::SharedMimeInfoXML::FreedesktopOrgDB->get_xml;

Returns a reference to the XML string from C<freedesktop.org.xml> distributed
with this module.

=cut

sub get_xml {
    (my $xml_name = __FILE__) =~ s!FreedesktopOrgDB\.pm$!mime-info/freedesktop.org.xml!;
    open my $fh, '<', $xml_name
        or die "Couldn't read '$xml_name': $!";
    binmode $fh;
    local $/;
    return \<$fh>
}

1;