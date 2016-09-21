package MIME::Detect;
use Moo;
use if $] < 5.022, 'Filter::signatures';
use feature 'signatures';
no warnings 'experimental::signatures';
use Carp qw(croak);
use XML::LibXML;

use vars '$VERSION';
$VERSION = '0.02';

=head1 NAME

MIME::Detect - MIME file type identification

=head1 SYNOPSIS

  my $mime = MIME::Detect->new();

  for my $file (@ARGV) {
    print sprintf "%s: %s\n", $file, $_->mime_type
        for $mime->mime_types($file);
  };

=head1 METHODS

=head2 C<< MIME::Detect->new( ... ) >>

  my $mime = MIME::Detect->new();

Creates a new instance and reads the database distributed with this module.

  my $mime = MIME::Detect->new(
      files => [
          '/usr/share/freedesktop.org/mimeinfo.xml',
          't/mimeinfo.xml',
      ],
  );

=cut

sub BUILD( $self, $args ) {
    my %db_args = map { exists( $args->{$_} )? ($_ => $args->{$_}) : () } (qw(xml files));
    $self->read_database( %db_args );
}

has 'typeclass' => (
    is => 'ro',
    default => 'MIME::Detect::Type',
);

has 'types' => (
    is => 'rw',
    default => sub { [] },
);

# References into @types
has 'known_types' => (
    is => 'rw',
    default => sub { {} },
);

has 'xpc' => (
    is => 'lazy',
    default => sub {
        my $XPC = XML::LibXML::XPathContext->new;
        $XPC->registerNs('x', 'http://www.freedesktop.org/standards/shared-mime-info');
        $XPC
    },
);

=head2 C<< $mime->read_database %options >>

  $mime->read_database(
      xml => MIME::Detect::FreeDesktopOrgDB->get_xml,
      files => [
          'mymime/mymime.xml',
          '/usr/share/freedesktop.org/mime.xml',
      ],
  );

If you want rules in addition to the default
database included with the distribution, you can load the rules from another file.
Passing in multiple filenames will join the multiple
databases. Duplicate file type definitions will not be detected
and will be returned as duplicates.

The rules will be sorted according to the priority specified in the database
file(s).

By default, the XML database stored alongside
L<MIME::Detect::FreeDesktopOrgDB>
will be loaded after all custom files have been loaded.
To pass in a different fallback database, either pass in a reference
to the XML string or the name of a package that has an C<get_xml> subroutine.

To prevent loading the default database, pass undef
for the C<xml> key.

=cut

sub read_database( $self, %options ) {
    $options{ files } ||= [];
    if( ! exists $options{ xml }) {
        $options{ xml } = 'MIME::Detect::FreeDesktopOrgDB';
    };
    
    if( $options{ xml } and not ref $options{ xml }) {
        # Load the class name
        if( !eval "require $options{ xml }; 1") {
            croak $@;
        };
        $options{ xml } = $options{ xml }->get_xml;
    };
    
    my @types = map {
        my @args = ref $_ eq 'SCALAR' ? (string   => $_) :
                   ref $_             ? (IO       => $_) :
                                        (location => $_);
        my $doc = XML::LibXML->load_xml(
            no_network => 1,
            load_ext_dtd => 0,
            @args
        );
        $self->_parse_types($doc);
    } @{$options{ files }}, $options{ xml };
    $self->reparse(@types);
}

sub _parse_types( $self, $document ) {
    map { $self->fragment_to_type( $_ ) }
    $self->xpc->findnodes('/x:mime-info/x:mime-type',$document);
}

sub reparse($self, @types) {
    @types = sort { ($b->priority || 50 ) <=> ($a->priority || 50 ) }
             @types;
    $self->types(\@types);

    # Build the map from mime_type to object
    my %mime_map;
    for my $t (@types) {
        $mime_map{ $t->mime_type } = $t;
        for my $a (@{$t->aliases}) {
            $mime_map{ $a } ||= $t;
        };
    };
    $self->known_types(\%mime_map);

    # Now, upgrade the strings to objects:
    my $m = $self->known_types;
    for my $t (@types) {
        my $s = $t->superclass;
        if( $s ) {
            if( my $sc = $m->{ $s } ) {
                $t->superclass( $sc );
            } else {
                warn sprintf "No superclass found for '%s' used by '%s'",
                    $s,
                    $t->mime_type;
            };
        };
    };
};

sub fragment_to_type( $self, $frag ) {
    my $mime_type = $frag->getAttribute('type');
    my $comment = $self->xpc->findnodes('./x:comment', $frag);
    my @globs = map { $_->getAttribute('pattern')} $self->xpc->findnodes('./x:glob', $frag);
    (my $superclass) = $self->xpc->findnodes('./x:sub-class-of',$frag);
    $superclass = $superclass->getAttribute('type')
        if $superclass;

    my @aliases = map { $_->getAttribute('type') } $self->xpc->findnodes('./x:alias',$frag);

    (my $magic) = $self->xpc->findnodes('./x:magic', $frag);
    my( $priority, @rules );
    if( $magic ) {
        $priority = $magic->getAttribute('priority');
        $priority = 50 if !defined $priority;
        @rules = grep { $_->nodeType != 3 } # exclude text nodes
                    $magic->childNodes;
        for my $rule (@rules) {
            $rule = $self->parse_rule( $rule );
        };
    };

    $self->typeclass->new(
        aliases => \@aliases,
        priority => $priority,
        mime_type => $mime_type,
        comment => $comment,
        superclass => $superclass,
        rules => \@rules,
        globs => \@globs,
    );
}

sub parse_rule( $self, $rule ) {
    my $value = $rule->getAttribute('value');
    my $offset = $rule->getAttribute('offset');
    my $type = $rule->getAttribute('type');

    my @and = map { $self->parse_rule( $_ ) } grep { $_->nodeType != 3 } $rule->childNodes;
    my $and = @and ? \@and : undef;

    return {
        value => $value,
        offset => $offset,
        type => $type,
        and => $and,
    };
}

=head2 C<< $mime->mime_types >>

    my @types = $mime->mime_types( 'some/file' );
    for( @types ) {
        print $type->mime_type, "\n";
    };

Returns the list of MIME types according to their likelyhood.
The first type is the most likely.

=cut

sub mime_types( $self, $file ) {
    if( ! ref $file) {
        open my $fh, '<', $file
            or croak "Couldn't read '$file': $!";
        binmode $fh;
        $file = $fh;
    };
    my $buffer = MIME::Detect::Buffer->new(fh => $file);
    $buffer->request(0,4096); # should be enough for most checks

    my @candidates;
    # We should respect the priorities here...
    my $m = $self->known_types;

    # Already sorted by priority
    my @types = @{ $self->{types} };

    # Let's just hope we don't have infinite subtype loops in the XML file
    for my $k (@types) {
        my $t = ref $k ? $k : $m->{ $k };
        if( $t->matches($buffer) ) {
            #warn sprintf "*** found '%s'", $t->mime_type;
            push @candidates, $m->{$t->mime_type};
        };
    };

    @candidates;
}

=head2 C<< $mime->mime_type >>

    my $type = $mime->mime_type( 'some/file' );
    print $type->mime_type, "\n"
        if $type;

Returns the most likely type of a file. Returns C<undef>
if no file type can be determined.

=cut

sub mime_type( $self, $file ) {
    ($self->mime_types($file))[0]
}

package MIME::Detect::Buffer;
use Moo;
use if $] < 5.022, 'Filter::signatures';
use feature 'signatures';
no warnings 'experimental::signatures';
use Fcntl 'SEEK_SET';

has 'offset' => (
    is => 'rw',
    default => 0,
);

has 'buffer' => (
    is => 'rw',
    default => undef,
);

has 'fh' => (
    is => 'ro',
);

sub length($self) {
    length $self->buffer || 0
};

sub request($self,$offset,$length) {
    my $fh = $self->fh;

    if( $offset =~ m/^(\d+):(\d+)$/) {
        $offset = $1;
        $length += $2;
    };

    #warn sprintf "At %d to %d (%d), want %d to %d (%d)",
    #         $self->offset, $self->offset+$self->length, $self->length,
    #         $offset, $offset+$length, $length;
    if(     $offset < $self->offset
        or  $self->offset+$self->length < $offset+$length ) {
        # We need to refill the buffer
        my $buffer;
        my $updated = 0;
        if (ref $fh eq 'GLOB') {
            if( seek($fh, $offset, SEEK_SET)) {
                read($fh, $buffer, $length);
                $updated = 1;
            };
        } else {
            # let's hope you have ->seek and ->read:
            if( $fh->seek($offset, SEEK_SET) ) {
                $fh->read($buffer, $length);
                $updated = 1;
            };
        }
        
        # Setting all three in one go would be more object-oriented ;)
        if( $updated ) {
            $self->offset($offset);
            $self->buffer($buffer);
        };
    };

    if(     $offset >= $self->offset
        and $self->offset+$self->length >= $offset+$length ) {
        substr $self->buffer, $offset-$self->offset, $length
    } elsif(     $offset >= $self->offset ) {
        substr $self->buffer, $offset-$self->offset
    } else {
        return ''
    };
}

1;

package MIME::Detect::Type;
use strict;
use Moo;
use if $] < 5.022, 'Filter::signatures';
use feature 'signatures';
no warnings 'experimental::signatures';

=head1 NAME

MIME::Detect::Type - the type of a file

=head1 SYNOPSIS

    my $type = $mime->mime_type('/usr/bin/perl');
    print $type->mime_type;
    print $type->comment;

=head1 METHODS

=cut

=head2 C<< $type->aliases >>

Reference to the aliases of this type

=cut

has 'aliases' => (
    is => 'ro',
    default => sub {[]},
);

=head2 C<< $type->comment >>

Array reference of the type description in various languages
(currently unused)

=cut

has 'comment' => (
    is => 'ro',
);

=head2 C<< $type->mime_type >>

    print "Content-Type: " . $type->mime_type . "\r\n";

String of the content type

=cut

has 'mime_type' => (
    is => 'ro',
);

=head2 C<< $type->globs >>

    print $_ for @{ $type->globs };

Arrayref of the wildcard globs of this type

=cut

has 'globs' => (
    is => 'ro',
    default => sub {[]},
);

sub _get_extension( $e=undef ) {
    if( defined $e ) { $e =~ s!^\*\.!! };
    $e
}

=head2 C<< $type->extension >>

    print $type->extension; # pl

Returns the default extension for this mime type, without a separating
dot or the glob.

=cut

sub extension($self) { 
    _get_extension( $self->globs->[0] );
}

=head2 C<< $type->valid_extension( $fn ) >>

    print "$fn has the wrong extension"
        unless $type->valid_extension( $fn );

Returns whether C<$fn> matches one of the extensions
as specified in C<globs>. If there is a match, the extension is returned.

=cut

sub valid_extension( $self, $fn ) {
    _get_extension((grep {
        my $g = $_;
        $g =~ s![.]!\\.!g;
        $g =~ s!\*!.*!g;
        $fn =~ /$g$/;
    } @{ $self->globs })[0])
}

=head2 C<< $type->priority >>

    print $type->priority;

Priority of this type. Types with higher priority
get tried first when trying to recognize a file type.

The default priority is 50.

=cut

has 'priority' => (
    is => 'ro',
    default => 50,
);

has 'rules' => (
    is => 'ro',
    default => sub { [] },
);

=head2 C<< $type->superclass >>

    my $sc = $type->superclass;
    print $sc->mime_type;

The notional superclass of this file type. Note that superclasses
don't necessarily match the same magic numbers.

=cut

has 'superclass' => (
    is => 'rw',
    default => undef,
);

sub BUILD($self, $args) {
    # Preparse the rules here:
    for my $rule (@{ $args->{rules} }) {
        my $value = $rule->{value};

        # This should go into the part reading the XML, not into the part
        # evaluating the rules
        if( ref $rule eq 'HASH' and $rule->{type} eq 'string' ) {
            my %replace = (
                'n' => "\n",
                'r' => "\r",
                't' => "\t",
                "\\" => "\\",
            );
            $value =~ s{\\([nrt\\]|([0-7][0-7][0-7])|x([0-9a-fA-F][0-9a-fA-F]))}
                       { $replace{$1} ? $replace{$1} 
                       : $2 ? chr(oct($2))
                       : $3 ? chr(hex($3))
                       : $1
                       }xge;

        } elsif( ref $rule eq 'HASH' and $rule->{type} eq 'little32' ) {
            $value = pack 'V', hex($rule->{value});

        } elsif( ref $rule eq 'HASH' and $rule->{type} eq 'little16' ) {
            $value = pack 'v', hex($rule->{value});

        } elsif( ref $rule eq 'HASH' and $rule->{type} eq 'big32' ) {
            $value = pack 'N', hex($rule->{value});

        } elsif( ref $rule eq 'HASH' and $rule->{type} eq 'big16' ) {
            $value = pack 'n', hex($rule->{value});

        } elsif( ref $rule eq 'HASH' and $rule->{type} eq 'host16' ) {
            $value = pack 'S', hex($rule->{value});

        } elsif( ref $rule eq 'HASH' and $rule->{type} eq 'host32' ) {
            $value = pack 'L', hex($rule->{value});

        } elsif( ref $rule eq 'HASH' and $rule->{type} eq 'byte' ) {
            $value = pack 'c', hex($rule->{value});

        } else {
            die "Unknown rule type '$rule->{type}'";
        };

        $rule->{type} = 'string';
        $rule->{value} = $value;
    }
}

sub compile($self,$fragment) {
    die "No direct-to-Perl compilation implemented yet.";
}

=head2 C<< $type->matches $buffer >>

    my $buf = "PK\003\004"; # first four bytes of file
    if( $type->matches( $buf ) {
        print "Looks like a " . $type->mime_type . " file";
    };

=cut

sub matches($self, $buffer, $rules = $self->rules) {
    my @rules = @$rules;

    # Superclasses are for information only
    #if( $self->superclass and $self->superclass->mime_type !~ m!^text/!) {
    #    return if ! $self->superclass->matches($buffer);
    #};

    if( !ref $buffer) {
        # Upgrade to an in-memory filehandle
        my $_buffer = $buffer;
        open my $fh, '<', \$_buffer
            or die "Couldn't open in-memory handle!";
        binmode $fh;
        $buffer = MIME::Detect::Buffer->new(fh => $fh);
    };

    # Hardcoded rule for plain text detection...
    if( $self->mime_type eq 'text/plain') {
        my $buf = $buffer->request(0,256);
        return $buf !~ /[\x00-\x08\x0b\x0c\x0e-\x1f]/;
    };

    my $matches;
    for my $rule (@rules) {

        my $value = $rule->{value};

        my $buf = $buffer->request($rule->{offset}, length $value);
        #use Data::Dumper;
        #$Data::Dumper::Useqq = 1;
        no warnings ('uninitialized', 'substr');
        if( $rule->{offset} =~ m!^(\d+):(\d+)$! ) {
            #warn sprintf "%s: index match %d:%d for %s", $self->mime_type, $1,$2, Dumper $value;
            #warn Dumper substr( $buf, 0, ($2-$1)+length($value));
            $matches = $matches || 1+index( substr( $buf, 0, ($2-$1)+length($value)), $value );
        } else {
            #warn sprintf "%s: substring match %d for %s", $self->mime_type, $rule->{offset}, Dumper $value;
            #warn Dumper substr( $buf, $rule->{offset}, length($value));
            $matches = $matches || substr( $buf, 0, length($value)) eq $value;
        };
        $matches = $matches && $self->matches( $buffer, $rule->{and} ) if $rule->{and};

        last if $matches;
    };
    !!$matches
}

1;

=head1 SEE ALSO

L<https://www.freedesktop.org/wiki/Software/shared-mime-info/> - the website
where the XML file is distributed

L<File::MimeInfo> - module to read your locally installed and converted MIME database

L<File::LibMagic> - if you can install C<libmagic> and the appropriate C<magic> files

L<File::MMagic> - if you have the appropriate C<magic> files

L<File::MMagic::XS> - if you have the appropriate C<magic> files but want more speed

L<File::Type> - inlines its database, unsupported since 2004?

L<File::Type::WebImages> - if you're only interested in determining whether
a file is an image or not

=head1 REPOSITORY

The public repository of this module is 
L<http://github.com/Corion/filter-signatures>.

=head1 SUPPORT

The public support forum of this module is
L<https://perlmonks.org/>.

=head1 BUG TRACKER

Please report bugs in this module via the RT CPAN bug queue at
L<https://rt.cpan.org/Public/Dist/Display.html?Name=Filter-signatures>
or via mail to L<filter-signatures-Bugs@rt.cpan.org>.

=head1 AUTHOR

Max Maischein C<corion@cpan.org>

=head1 COPYRIGHT (c)

Copyright 2015-2016 by Max Maischein C<corion@cpan.org>.

=head1 LICENSE

This module is released under the same terms as Perl itself.

=cut
