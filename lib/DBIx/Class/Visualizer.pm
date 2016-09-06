use 5.10.1;
use strict;
use warnings;

package DBIx::Class::Visualizer;

# ABSTRACT: Visualize a DBIx::Class schema
# AUTHORITY
our $VERSION = '0.0101';

use GraphViz2;
use Log::Handler;
use List::Util qw/any none/;
use DateTime::Tiny;
use Moo;
use Types::Standard qw/ArrayRef RegexpRef Maybe/;

has logger => (
    is => 'ro',
    default => sub {
        my $logger = Log::Handler->new;
        $logger->add(
            screen => {
                maxlevel => 'debug',
                minlevel => 'error',
                message_layout => '%m',
            },
        );
        return $logger;
    },
);
has graphviz_config => (
    is => 'ro',
    lazy => 1,
    default => sub {
        my $self = shift;
        my %label = $self->single_result_source
                  ? ()
                  : (label => sprintf ('%s (version %s) rendered by DBIx::Class::Visualizer %s.', ref $self->schema, $self->schema->schema_version, DateTime::Tiny->now->as_string))
                  ;

        return +{
            global => {
                directed => 1,
                smoothing => 'none',
                overlap => 'false',
                logger => $self->logger,
            },
            graph => {
                rankdir => 'LR',
                splines => 'true',
                %label,
                fontname => 'helvetica',
                fontsize => 8,
                labeljust => 'l',
                nodesep => 0.38,
                ranksep => 0.46,
            },
            node => {
                fontname => 'helvetica',
                shape => 'none',
            },
        };
    },
);
has graph => (
    is => 'ro',
    lazy => 1,
    builder => '_build_graph',
    handles => [qw/run/],
);
sub _build_graph {
    return GraphViz2->new(shift->graphviz_config);
}
has schema => (
    is => 'ro',
    required => 1,
);
has result_sources => (
    is => 'rw',
    isa => ArrayRef,
    lazy => 1,
    default => sub {
        [shift->schema->sources],
    },
);
has single_result_source => (
    is => 'ro',
    default => 0,
);
has degrees_of_separation => (
    is => 'ro',
    default => 1,
);

has skip_result_sources => (
    is => 'rw',
    isa => Maybe[RegexpRef],
    predicate => 1,
);


has added_relationships => (
    is => 'ro',
    default => sub { +{} },
);
has ordered_relationships => (
    is => 'ro',
    default => sub { [] },
);

around BUILDARGS => sub {
    my $orig = shift;
    my $class = shift;
    my %args = @_;

    # If we should display only one result source and the user has not passed single_result_source => 0, then single_result_source => 1.
    if(exists $args{'result_sources'} && scalar @{ $args{'result_sources'} } && !exists $args{'single_result_source'}) {
        $args{'single_result_source'} = 1;
    }

    return $class->$orig(%args);
};

sub BUILD {
    my $self = shift;
    my @sources = grep { my $regex = $self->skip_result_sources; !($self->has_skip_result_sources && m/$regex/) } @{ $self->result_sources };

    $self->logger->debug('sources ', @sources);

    if($self->single_result_source) {
        for (1..$self->degrees_of_separation) {
            say '->';
            my @add_sources;
            SOURCE:
            for my $source_name (@sources) {
                say "sources:    @sources";
                say "-->       $source_name";
                next SOURCE if $self->has_skip_result_sources && do { my $regex = $self->skip_result_sources; $source_name =~ m/$regex/; };
                my $rs = $self->schema->resultset($source_name)->result_source;

                RELATION:
                for my $relation_name (sort $rs->relationships) {
                    my $relation = $rs->relationship_info($relation_name);
                    (my $other_source_name = $relation->{'class'}) =~ s{^.*?::Result::}{};

                    next RELATION if $self->has_skip_result_sources && do { my $regex = $self->skip_result_sources; $other_source_name =~ m/$regex/; };
                    next RELATION if any { $other_source_name eq $_ } (@sources, @add_sources);

                    say "----->             $relation_name -> $other_source_name";
                    push @add_sources, $other_source_name;
                }
                say '';
            }
            push @sources, @add_sources;
        }
        $self->result_sources(\@sources);
    }
    else {
        @sources = sort @sources;
    }

    foreach my $source_name (@sources) {
        $self->add_node($source_name);
    }
    foreach my $source_name (@sources) {
        $self->add_edges($source_name);
    }
}

sub svg {
    my $self = shift;

    my $output;
    $self->graph->run(output_file => \$output, format => 'svg');

    # remove padding
    $output =~ s{<text [^>]*fill="white"[^>]*>_*?</text>}{}g;

    # first add a thinner stroke-width to the last polygon in each <g> (the border around each table)
    $output =~ s{ <polygon [^>]*\K />(?=[^<]* </g>)}{ stroke-width="0.4"/>}mxg;
    # and then remove stroke-width's for a marked polygon (stroke-width="3")
    $output =~ s{ <polygon [^>]* \K stroke-width="3" ([^>]*) stroke-width="0.4" />(?=[^<]* </g>)}{ $1 />}mxg;

    # make it a bit lighter
    $output =~ s{(fill|stroke)="black"}{$1="#444444"}g;

    # Fix the graph name
    my $schema_name = ref $self->schema;
    $output =~ s{<title>Perl</title>}{<title>$schema_name</title>};

    # Cleanup edge alt texts
    $output =~ s{ <title>\K (?<origin_table>[^:]+) : (?<origin_column>[^&]+?) &.*? (?<destination_table>[^:;]+) : (?<destination_column>[^<]+) (?=</title>) }{$+{'origin_table'}.$+{'origin_column'} &#45;&gt; $+{'destination_table'}.$+{'destination_column'}}xg;
    return $output;
}

sub add_node {
    my $self = shift;
    my $source_name = shift;

    my $node_name = $self->node_name($source_name);
    my $rs = $self->schema->resultset($source_name)->result_source;

    my @primary_columns = $rs->primary_columns;
    my @foreign_columns = map { keys %{ $_->{'attrs'}{'fk_columns'} } } map { $rs->relationship_info($_) } $rs->relationships;

    my $label_data = {
        source_name => $source_name,
        columns => [],
    };
    for my $column ($rs->columns) {
        my $is_primary = any { $column eq $_ } @primary_columns;
        my $is_foreign = any { $column eq $_ } @foreign_columns;
        push @{ $label_data->{'columns'} } => {
            is_primary => $is_primary,
            is_foreign => $is_foreign,
            name => $column,
        };
    }
    $self->graph->add_node(
        name => $node_name,
        label => $self->create_label_html($node_name, $label_data, { mark_label => $self->single_result_source && $source_name eq $self->result_sources->[0] ? 1 : 0 }),
        margin => 0.01,
    );
}

sub add_edges {
    my $self = shift;
    my $source_name = shift;

    my $rs = $self->schema->resultset($source_name)->result_source;

    RELATION:
    for my $relation_name (sort $rs->relationships) {
        my $node_name = $self->node_name($source_name);
        $self->logger->info('relation name: ' . $relation_name);
        my $relation = $rs->relationship_info($relation_name);
        (my $other_source_name = $relation->{'class'}) =~ s{^.*?::Result::}{};

        next RELATION if $self->has_skip_result_sources && do { my $regex = $self->skip_result_sources; $other_source_name =~ m/$regex/; };
        my $other_node_name = $self->node_name($other_source_name);

        # Have we already added the edge from the reversed direction?
        next RELATION if exists $self->added_relationships->{"$other_node_name-->$node_name"};

        my $other_rs = $self->schema->resultset($other_source_name)->result_source;
        my $other_relation;

        OTHER_RELATION:
        for my $other_relation_name ($other_rs->relationships) {
            my $relation_to_attempt = $other_rs->relationship_info($other_relation_name);

            (my $possibly_original_class = $relation_to_attempt->{'class'}) =~ s{^.*?::Result::}{};
            next OTHER_RELATION if $possibly_original_class ne $source_name;

            # When we are rendering *part* of the schema, result_sources() doesn't contain all result sources in the schema, but only those we will display.
            # If the current relation points to a result source outside the wanted degrees_of_separation, we are not interested.
            next RELATION if $self->single_result_source && none { $other_source_name eq $_ } @{ $self->result_sources };

            $other_relation = $relation_to_attempt;
            $other_relation->{'_name'} = $other_relation_name;
        }

        if(!defined $other_relation) {
            warn "! No reverse relationship $source_name <-> $other_source_name";
            next RELATION;
        }

        my $arrowhead = $self->get_arrow_type($relation);
        my $arrowtail = $self->get_arrow_type($other_relation);

        my $headport = ref $relation->{'cond'} eq 'HASH' && scalar keys %{ $relation->{'cond'} } == 1
                             ? (keys %{ $relation->{'cond'} })[0] =~ s{^foreign\.}{}rx
                             : $node_name
                             ;
        my $tailport = ref $relation->{'cond'} eq 'HASH' && scalar keys %{ $relation->{'cond'} } == 1
                             ? (values %{ $relation->{'cond'} })[0] =~ s{^self\.}{}rx
                             : $node_name
                             ;


        # When displaying a single result source, and its closest relations, place some relations to the left and some to the right.
        # The placement is dependent on the direction of the connection. Hence: invert some relations.
        if($self->single_result_source) {
            if(any { $_ eq $self->get_relation_type($relation) } (qw/belongs_to has_one/)) {
                my $placeholder_node = $node_name;
                $node_name = $other_node_name;
                $other_node_name = $placeholder_node;

                my $placeholder_arrow = $arrowhead;
                $arrowhead = $arrowtail;
                $arrowtail = $placeholder_arrow;

                my $placeholder_port = $headport;
                $headport = $tailport;
                $tailport = $placeholder_port;
            }
        }

        $self->graph->add_edge(
            from => $node_name,
            tailport => $tailport,
            to => $other_node_name,
            headport => $headport,
            arrowhead => $arrowhead,
            arrowtail => $arrowtail,
            dir => 'both',
            minlen => 2,
            penwidth => 2,
        );

        $self->added_relationships->{ "$node_name-->$other_node_name" } = 1;
        $self->added_relationships->{ "$other_node_name-->$node_name" } = 1;

        push @{ $self->ordered_relationships } => (
            "$node_name-->$other_node_name",
            "$other_node_name-->$node_name"
        );
    }
}

sub get_relation_type {
    my $self = shift;
    my $relation = shift;

    my $accessor = $relation->{'attrs'}{'accessor'};
    my $is_depends_on = $relation->{'attrs'}{'is_depends_on'};
    my $join_type = exists $relation->{'attrs'}{'join_type'} ? lc $relation->{'attrs'}{'join_type'} : '';

    my $has_many   = $accessor eq 'multi'  && !$is_depends_on && $join_type eq 'left' ? 1 : 0;
    my $belongs_to = $accessor eq 'single' && $is_depends_on  && $join_type eq ''     ? 1 : 0;
    my $might_have = $accessor eq 'single' && !$is_depends_on && $join_type eq 'left' ? 1 : 0;

    return $has_many   ? 'has_many'
         : $belongs_to ? 'belongs_to'
         : $might_have ? 'might_have'
         :               'unknown'
         ;
}
sub get_arrow_type {
    my $self = shift;
    my $relation = shift;

    my $relation_type = $self->get_relation_type($relation);

    return $relation_type eq 'has_many'   ? join ('' => qw/crow none odot/)
         : $relation_type eq 'belongs_to' ? join ('' => qw/none tee/)
         : $relation_type eq 'might_have' ? join ('' => qw/none tee none odot/)
         :                                  join ('' => qw/dot dot dot/)
         ;
}
sub get_port_compass {
    my $self = shift;
    my $relation = shift;
    my $is_origin = shift;

    return '' if !$self->single_result_source;
    my $relation_type = $self->get_relation_type($relation);

    return $relation_type eq 'has_many'   ? $is_origin ? ':e' : ':w'
         : $relation_type eq 'belongs_to' ? $is_origin ? ':w' : ':e'
         : $relation_type eq 'might_have' ? $is_origin ? ':w' : ':e'
         :                                  ''
         ;
}

sub node_name {
    my $self = shift;
    my $node_name = shift;
    $node_name =~ s{::}{__}g;
    return $node_name;
}
sub port_name {
    my $self = shift;
    my $source_name = shift;
    my $column_name = shift;

    my $node_name = $self->node_name($source_name);
    return "$node_name--$column_name";
}

sub create_label_html {
    my $self = shift;
    my $node_name = shift;
    my $data = shift;
    my $args = shift;

    my $mark_label = $args->{'mark_label'} || 0;

    my $column_html = [];

    for my $column (@{ $data->{'columns'} }) {
        my $clean_column_name = my $column_name = $column->{'name'};

        my $port_name = $self->port_name($node_name, $column_name);

        $column_name = $column->{'is_primary'} ? "<b>$column_name</b>" : $column_name;
        $column_name = $column->{'is_foreign'} ? "<u>$column_name</u>" : $column_name;

        push @{ $column_html } => qq{
            <tr><td align="left" port="$clean_column_name" bgcolor="#fefefe"> <font point-size="12" color="#222222">$column_name</font><font color="white">_@{[ $self->padding($column_name) ]}</font></td></tr>};
    }
    my $html = qq{
        <<table cellborder="0" cellpadding="1" cellspacing="0" border="@{[ $mark_label ? '3' : '1' ]}" width="150">
            <tr><td bgcolor="#DDDFDD" width="150"><font point-size="1"> </font></td></tr>
            <tr><td align="left" bgcolor="#DDDFDD"> <font color="#333333"><b>$data->{'source_name'}</b></font><font color="white">_@{[ $self->padding($data->{'source_name'}) ]}</font></td></tr>
            <tr><td><font point-size="3"> </font></td></tr>
            } . join ('', @{ $column_html }) . qq{
        </table>>
    };

    return $html;
}

# graphviz (at least sometimes) draws too small boxes. We pad them a little (and remove the padding in svg())
sub padding {
    my $self = shift;
    my $text = shift;

    return '_' x int (length ($text) / 10);
}

1;

__END__

=pod

=head1 SYNOPSIS

    use DBIx::Class::Visualizer;
    use A::DBIx::Class::Schema;

    my $schema = A::DBIx::Class::Schema->connect;
    my $svg = DBIx::Class::Visualizer->new->svg;

=head1 DESCRIPTION

DBIx::Class::Visualizer is a L<GraphViz2> renderer for L<DBIx::Class> schemata.

On the relatively small schemata (about twenty result classes) that I have tried it on it produces reasonably readable graphs. See C<example/visualized.svg> for a
simple example (also available on L<Github|http://htmlpreview.github.io/?https://github.com/Csson/p5-DBIx-Class-Visualizer/blob/master/example/visualized.svg>).

=head1 ATTRIBUTES

=head2 schema

Required. An instance of a L<DBIx::Class::Schema> class.

=head2 graphviz_config

Optional hashref. This hashref is passed to the L<GraphViz2> constructor. Set this if the defaults don't work. Setting this will replace the defaults.

Won't be used if you pass C<graph> to the constructor.

=head2 graph

Optional. A L<GraphViz2> object. Set this if you need to use an already constructed graph.

It can be useful if you, for example, wishes to see the arguments to the dot renderer:

    my $visualizer = DBIx::Class::Visualizer->new(schema => $schema);
    my $svg = $visualizer->svg;

    my $dotfile = $visualizer->graph->dot_input;


=head1 METHODS

=head2 new

The constructor.

=head2 svg

Takes no arguments, and returns the rendered svg document as a string.

=head2 run

A shortcut for L<GraphViz2/run>:

    DBIx::Class::Visualizer->new(schema => $schema)->run(output_file => 'myschema.png', format => 'png');


=head1 SEE ALSO

=for :list
* L<Mojolicious::Plugin::DbicSchemaViewer> - A L<Mojolicious> plugin that uses this class
* L<GraphViz2::DBI> - Visualizes a schema by inspecting the database.

=cut
