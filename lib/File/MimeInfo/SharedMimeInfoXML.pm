package File::MimeInfo::SharedMimeInfoXML;
use Moo;
use if $] < 5.022, 'Filter::signatures';
use feature 'signatures';
no warnings 'experimental::signatures';
use Carp qw(croak);
use XML::LibXML;

use vars '$VERSION';
$VERSION = '0.01';

=head1 NAME

File::MimeInfo::SharedMimeInfoXML - file type identification from the freedesktop.org database

=head1 TO DO

before release:

* Find a better name or third part

* Revisit API compatibility with other MimeInfo modules

* Bundle the XML inline

* Write updater for updating the distributed XML file

* Make C<mime-info.pl> commandline compatible with C<file>

=head1 SYNOPSIS

  my $mime = File::MimeInfo::SharedMimeInfoXML->new();

  for my $file (@ARGV) {
    print sprintf "%s: %s\n", $file, $_->mime_type
        for $mime->mimetype($file);
  };

=head1 METHODS

=head2 C<< File::MimeInfo::SharedMimeInfoXML->new( ... ) >>

  my $mime = File::MimeInfo::SharedMimeInfoXML->new();

Creates a new instance and reads the database distributed with this module.

  my $mime = File::MimeInfo::SharedMimeInfoXML->new(
      database => [
          '/usr/share/freedesktop.org/mimeinfo.xml',
          't/mimeinfo.xml',
      ],
  );

=cut

sub BUILD( $self, $args ) {
    if( ref $args->{database} and @{ $args->{database} }) {
        $self->read_database( @{ $args->{database}} )
    };
}

has 'typeclass' => (
    is => 'ro',
    default => 'File::MimeInfo::SharedMimeInfoXML::Type',
);

has 'types' => (
    is => 'rw',
    default => sub { [] },
);

# References into @types
has 'mime_types' => (
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

=head2 C<< $mime->read_database @files >>

  $mime->read_database('mymime/mymime.xml','/usr/share/freedesktop.org/mime.xml');

If you want some different rules than the default
database included with the distribution, you can replace the
database by a database stored in another file.
Passing in multiple filenames will join the multiple
databases. Duplicate file type definitions will not be detected
and will be returned as duplicates.

The rules will be sorted according to the priority specified in the database
file(s).

=cut

sub read_database( $self, @files ) {
    my @types = map {
        my $p = XML::LibXML->new();
        my $doc = $p->parse_file( $_ );
        $self->_parse_types($doc);
    } @files;
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
    $self->mime_types(\%mime_map);

    # Now, upgrade the strings to objects:
    my $m = $self->mime_types;
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
    my @globs = $self->xpc->findnodes('./x:glob', $frag);
    (my $superclass) = $self->xpc->findnodes('./x:sub-class-of',$frag);
    $superclass = $superclass->getAttribute('type')
        if $superclass;

    my @aliases = map { $_->getAttribute('type') } $self->xpc->findnodes('./x:alias',$frag);

    (my $magic) = $self->xpc->findnodes('./x:magic', $frag);
    my( $priority, @rules );
    if( $magic ) {
        $priority = $magic->getAttribute('priority');
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

=head2 C<< $mime->mimetypes >>

    my @types = $mime->mimetypes( 'some/file' );
    for( @types ) {
        print $type->mime_type, "\n";
    };

Returns the list of mimetypes according to their likelyhood.
The first type is the most likely.

=cut

sub mimetypes( $self, $file ) {
    if( ! ref $file) {
        open my $fh, '<', $file
            or croak "Couldn't read '$file': $!";
        binmode $fh;
        $file = $fh;
    };
    my $buffer = File::MimeInfo::SharedMimeInfoXML::Buffer->new(fh => $file);
    $buffer->request(0,4096); # should be enough for most checks

    my @candidates;
    # We should respect the priorities here...
    my $m = $self->mime_types;

    my @types = @{ $self->{types} };

    # Let's just hope we don't have infinite subtype loops in the XML file
    for my $k (@types) {
        my $t = ref $k ? $k : $m->{ $k };
        if( $t->matches($buffer) ) {
            #warn sprintf "*** found '%s'", $t->mime_type;
            push @candidates, $m->{$t->mime_type};
        };
    };

    # Now, sort by priority of the rules that matched?!
    reverse @candidates;
}

=head2 C<< $mime->mimetype >>

    my $type = $mime->mimetype( 'some/file' );
    print $type->mime_type, "\n"
        if $type;

Returns the most likely type of a file. Returns C<undef>
if no file type can be determined.

=cut

sub mimetype( $self, $file ) {
    ($self->mimetypes($file))[0]
}

package File::MimeInfo::SharedMimeInfoXML::Buffer;
use Moo;
use if $] < 5.022, 'Filter::signatures';
use feature 'signatures';
no warnings 'experimental::signatures';
use Fcntl 'SEEK_SET';

has 'offset' => (
    is => 'rw',
    default => 0,
);

has 'length' => (
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

sub request($self,$offset,$length) {
    my $fh = $self->fh;

    if( $offset =~ m/^(\d+):(\d+)$/) {
        $offset = $1;
        $length += $2;
    };

    if(     $offset < $self->offset
        or  $self->offset+$self->length < $offset+$length ) {
        # We need to refill the buffer
        my $buffer;
        if (ref $fh eq 'GLOB') {
            seek($fh, $offset, SEEK_SET);
            read($fh, $buffer, $length);
        } else {
            # let's hope you have ->seek and ->read:
            $fh->seek($offset, SEEK_SET);
            $fh->read($buffer, $length);
        }

        # Setting all three in one go would be more object-oriented ;)
        $self->offset($offset);
        $self->length($length);
        $self->buffer($buffer);
    };

    $self->buffer
}

1;

package File::MimeInfo::SharedMimeInfoXML::Type;
use strict;
use Moo;
use if $] < 5.022, 'Filter::signatures';
use feature 'signatures';
no warnings 'experimental::signatures';

has 'aliases' => (
    is => 'ro',
    default => sub {[]},
);

has 'comment' => (
    is => 'ro',
);

has 'mime_type' => (
    is => 'ro',
);

has 'globs' => (
    is => 'ro',
    default => sub {[]},
);

has 'priority' => (
    is => 'ro',
    default => 50,
);

has 'rules' => (
    is => 'ro',
    default => sub { [] },
);

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
                '\\' => "\\",
            );
            $value =~ s!\\([nrt\\]|0\d\d)!$replace{$1} || chr(oct($1)) !ge;

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
        $buffer = File::MimeInfo::SharedMimeInfoXML::Buffer->new(fh => $fh);
    };

    # Hardcoded rule for plain text detection...
    if( $self->mime_type eq 'text/plain') {
        my $buf = $buffer->request(0,256);
        return $buf !~ /[\x00-\x08\x0b\x0c\x0e-\x1f]/;
    };

    my $matches;
    for my $rule (@rules) {

        my $value = $rule->{value};

        no warnings ('uninitialized', 'substr');
        my $buf = $buffer->request($rule->{offset}, length $value);
        #use Data::Dumper;
        #$Data::Dumper::Useqq = 1;
        if( $rule->{offset} =~ m!^(\d+):(\d+)$! ) {
            #warn "index match $1:$2 for " . Dumper $value;
            #warn Dumper substr( $buf, $1, $2+length($value));
            $matches = $matches || 1+index( substr( $buf, $1, $2+length($value)), $value );
        } else {
            #warn "substring match $rule->{offset} for " . Dumper $value;
            #warn Dumper substr( $buf, $rule->{offset}, length($value));
            $matches = $matches || substr( $buf, $rule->{offset}, length($value)) eq $value;
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

=cut