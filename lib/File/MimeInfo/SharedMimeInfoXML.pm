package File::MimeInfo::SharedMimeInfoXML;
use Moo;
use feature 'signatures';
no warnings 'experimental::signatures';
use Carp qw(croak);
use XML::LibXML;
  
use vars '$XPC';
$XPC = XML::LibXML::XPathContext->new;
$XPC->registerNs('x', 'http://www.freedesktop.org/standards/shared-mime-info');

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

sub read_file( $self, $file ) {
    my $p = XML::LibXML->new();
    my $doc = $p->parse_file( $file );
    $self->reparse($doc);
}

sub reparse($self, $document) {
    my @types = sort { ($b->priority || 50 ) <=> ($a->priority || 50 ) }
                map { $self->fragment_to_type( $_ ) }
                $XPC->findnodes('/x:mime-info/x:mime-type',$document);
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
    my $comment = $XPC->findnodes('./x:comment', $frag);
    my @globs = $XPC->findnodes('./x:glob', $frag);
    (my $superclass) = $XPC->findnodes('./x:sub-class-of',$frag);
    $superclass = $superclass->getAttribute('type')
        if $superclass;
    
    my @aliases = map { $_->getAttribute('type') } $XPC->findnodes('./x:alias',$frag);

    (my $magic) = $XPC->findnodes('./x:magic', $frag);
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

sub mimetype( $self, $file ) {
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
    @candidates;
}

package File::MimeInfo::SharedMimeInfoXML::Buffer;
use Moo;
use if $] < 5.022_000, 'Filter::signatures';
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

    # XXX everything is a text/plain file...
    return 1 if $self->mime_type eq 'text/plain';

    my $matches;
    for my $rule (@rules) {
        
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
            use Data::Dumper;
            die "Unknown rule format: " . Dumper $rule;
        };

        my $buf = $buffer->request($rule->{offset}, length $value);
        if( $rule->{offset} =~ m!^(\d+):(\d+)$! ) {
            #warn "index match $1:$2 for $value";
            #warn $buf;
            #warn substr( $buf, $1, $2+length($value));
            $matches = $matches || 1+index( substr( $buf, $1, $2+length($value)), $value );
        } else {
            #warn "substring match $rule->{offset} for $value";
            $matches = $matches || substr( $buf, $rule->{offset}, length($value)) eq $value;
        };
        $matches = $matches && $self->matches( $buffer, $rule->{and} ) if $rule->{and};

        last if $matches;
    };
    !!$matches
}

1;

__END__
  <!ELEMENT mime-info (mime-type)+>
  <!ATTLIST mime-info xmlns CDATA #FIXED "http://www.freedesktop.org/standards/shared-mime-info">

  <!ELEMENT mime-type (comment+, (acronym,expanded-acronym)? , (generic-icon? | glob | magic | treemagic | root-XML | alias | sub-class-of)*)>
  <!ATTLIST mime-type type CDATA #REQUIRED>

  <!-- a comment describing a document with the respective MIME type. Example: "WMV video" -->
  <!ELEMENT comment (#PCDATA)>
  <!ATTLIST comment xml:lang CDATA #IMPLIED>

  <!-- a comment describing the respective unexpanded MIME type acronym. Example: "WMV" -->
  <!ELEMENT acronym (#PCDATA)>
  <!ATTLIST acronym xml:lang CDATA #IMPLIED>

  <!-- a comment describing the respective expanded MIME type acronym. Example: "Windows Media Video" -->
  <!ELEMENT expanded-acronym (#PCDATA)>
  <!ATTLIST expanded-acronym xml:lang CDATA #IMPLIED>

  <!-- a generic icon name as per the Icon Naming Specification, only required if computing
  it from the mime-type would not work, See "generic-icon" in the Shared Mime Specification -->
  <!ELEMENT generic-icon EMPTY>
  <!ATTLIST generic-icon name (application-x-executable|audio-x-generic|font-x-generic|image-x-generic|package-x-generic|text-html|text-x-generic|text-x-generic-template|text-x-script|video-x-generic|x-office-address-book|x-office-calendar|x-office-document|x-office-presentation|x-office-spreadsheet) #IMPLIED>

  <!ELEMENT glob EMPTY>
  <!ATTLIST glob pattern CDATA #REQUIRED>
  <!ATTLIST glob weight CDATA #IMPLIED>
  <!ATTLIST glob case-sensitive CDATA #IMPLIED>

  <!ELEMENT magic (match)+>
  <!ATTLIST magic priority CDATA #IMPLIED>

  <!ELEMENT match (match)*>
  <!ATTLIST match offset CDATA #REQUIRED>
  <!ATTLIST match type (string|big16|big32|little16|little32|host16|host32|byte) #REQUIRED>
  <!ATTLIST match value CDATA #REQUIRED>
  <!ATTLIST match mask CDATA #IMPLIED>

  <!ELEMENT treemagic (treematch)+>
  <!ATTLIST treemagic priority CDATA #IMPLIED>

  <!ELEMENT treematch (treematch)*>
  <!ATTLIST treematch path CDATA #REQUIRED>
  <!ATTLIST treematch type (file|directory|link) #IMPLIED>
  <!ATTLIST treematch match-case (true|false) #IMPLIED>
  <!ATTLIST treematch executable (true|false) #IMPLIED>
  <!ATTLIST treematch non-empty (true|false) #IMPLIED>
  <!ATTLIST treematch mimetype CDATA #IMPLIED>

  <!ELEMENT root-XML EMPTY>
  <!ATTLIST root-XML namespaceURI CDATA #REQUIRED>
  <!ATTLIST root-XML localName CDATA #REQUIRED>

  <!ELEMENT alias EMPTY>
  <!ATTLIST alias type CDATA #REQUIRED>

  <!ELEMENT sub-class-of EMPTY>
  <!ATTLIST sub-class-of type CDATA #REQUIRED>
