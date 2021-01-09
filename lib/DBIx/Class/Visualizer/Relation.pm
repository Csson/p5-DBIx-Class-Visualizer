use 5.10.1;
use strict;
use warnings;

package DBIx::Class::Visualizer::Relation;

# ABSTRACT: Handle relation information
# AUTHORITY
our $VERSION = '0.0201';

use Moo;
use Types::Standard qw/Str Bool Enum/;
use PerlX::Maybe qw/provided/;

our %RELATION_TO_ARROW = (
    has_many => join('', qw/crow none odot/),
    belongs_to => join('', qw/none tee/),
    might_have => join('', qw/none tee none odot/),
    has_one => join('', qw/vee/),
);

has added_to_graph => (
    is => 'rw',
    isa => Bool,
    default => 0,
);

has origin_table => (
    is => 'ro',
    isa => Str,
    required => 1,
);
has origin_column => (
    is => 'ro',
    isa => Str,
    required => 1,
);
has destination_table => (
    is => 'ro',
    isa => Str,
    required => 1,
);
has destination_column => (
    is => 'ro',
    isa => Str,
    required => 1,
);
has cascade_delete => (
    is => 'ro',
    isa => Bool,
    predicate => 1,
);
has cascade_update => (
    is => 'ro',
    isa => Bool,
    predicate => 1,
);

has relation_type => (
    is => 'ro',
    isa => Enum[ keys %RELATION_TO_ARROW ],
    required => 1,
);

around BUILDARGS => sub {
    my $orig = shift;
    my $class = shift;
    my %args = @_;

    my $relation = delete $args{'relation'};
    my $attr = $relation->{'attrs'};

    ($args{'destination_table'} = $relation->{'source'}) =~ s{^.*?::Result::}{};
    ($args{'origin_column'} = (values %{ $relation->{'cond'} })[0]) =~ s{^self\.}{};
    ($args{'destination_column'} = (keys %{ $relation->{'cond'} })[0]) =~ s{^foreign\.}{};

    for my $cascade (qw/cascade_delete cascade_update/) {
        $args{ $cascade } = $attr->{ $cascade } if exists $attr->{ $cascade };
    }

    $args{'relation_type'} = _attr2relation_type($attr);

    $class->$orig(%args);
};

sub _attr2relation_type {
    my ($attr) = @_;
    # do not reorder
    $attr->{'accessor'} eq 'multi'    ? 'has_many'
        : $attr->{'is_depends_on'}    ? 'belongs_to'
        : exists $attr->{'join_type'} ? 'might_have'
        :                               'has_one'
        ;
}

sub is_belongs_to { shift->relation_type eq 'belongs_to' }

sub arrow_type {
    my $self = shift;
    $RELATION_TO_ARROW{ $self->relation_type };
}

sub TO_JSON {
    my $self = shift;

    return +{
            origin_table => $self->origin_table,
            origin_column => $self->origin_column,
            destination_table => $self->destination_table,
            destination_column => $self->destination_column,
            relation_type => $self->relation_type,
        provided $self->has_cascade_delete,
            cascade_delete => $self->cascade_delete,
        provided $self->has_cascade_update,
            cascade_update => $self->cascade_update,
    };
}

1;
