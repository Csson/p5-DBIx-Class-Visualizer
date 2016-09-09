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
use Mojo::DOM;
use Mojo::Util qw/trim/;
use Types::Standard qw/ArrayRef RegexpRef Maybe/;
use Syntax::Keyword::Gather;
use JSON::MaybeXS qw/encode_json/;

has logger_config => (
    is => 'ro',
    isa => ArrayRef,
    default => sub {
        [
            screen => {
                maxlevel => 'debug',
                message_layout => '%m',
            },
        ],
    },
);

has logger => (
    is => 'ro',
    lazy => 1,
    default => sub {
        my $logger = Log::Handler->new(@{ shift->logger_config });
        return $logger;
    },
);
has graphviz_config => (
    is => 'ro',
    lazy => 1,
    default => sub {
        my $self = shift;
        my %label = $self->has_wanted_result_sources
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
                fontsize => 7,
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
    init_arg => undef,
    builder => 1,
);
sub _build_result_sources {
    my $self = shift;

    return $self->has_wanted_result_sources ? $self->wanted_result_sources : [$self->schema->sources];
};
has wanted_result_sources => (
    is => 'ro',
    isa => ArrayRef,
    default => sub { [] },
);

has degrees_of_separation => (
    is => 'ro',
    default => 1,
);

has skip_result_sources => (
    is => 'rw',
    isa => ArrayRef,
    default => sub { [] },
);

has added_relationships => (
    is => 'ro',
    default => sub { +{} },
);

sub has_wanted_result_sources { scalar @{ shift->wanted_result_sources }; }
sub has_skip_result_sources   { scalar @{ shift->skip_result_sources }; }

sub BUILD {
    my $self = shift;
    my @sources = sort grep { my $result_source = $_; none { $result_source eq $_ } @{ $self->skip_result_sources } } @{ $self->result_sources };

    if($self->has_wanted_result_sources) {
        for (1..$self->degrees_of_separation) {

            push @sources, gather {
                SOURCE:
                for my $source_name (@sources) {
                    next SOURCE if $self->has_skip_result_sources && any { $source_name eq $_ } @{ $self->skip_result_sources };
                    my $rs = $self->schema->resultset($source_name)->result_source;

                    RELATION:
                    for my $relation_name (sort $rs->relationships) {
                        my $relation = $rs->relationship_info($relation_name);
                        (my $other_source_name = $relation->{'class'}) =~ s{^.*?::Result::}{};

                        next RELATION if $self->has_skip_result_sources && any { $other_source_name eq $_ } @{ $self->skip_result_sources };
                        next RELATION if any { $other_source_name eq $_ } (@sources, gathered);
                        take $other_source_name;
                    }
                }
            };
        }
    }

    $self->result_sources(sort \@sources);

    foreach my $source_name (@{ $self->result_sources }) {
        $self->add_node($source_name);
    }
    foreach my $source_name (@{ $self->result_sources }) {
        $self->add_edges($source_name);
    }
}

sub svg {
    my $self = shift;

    my $output;
    $self->graph->run(output_file => \$output, format => 'svg');

    my $dom = Mojo::DOM->new($output);

    # remove elements used for padding
    $dom->find('text[fill="white"]')->each(sub {
        my $el = shift;
        $el->remove if $el->text =~ m{^_+$};
    });
    $dom->find('text')->each(sub {
        my $el = shift;
        $el->remove if !length trim $el->text;
    });

    # Remove attributes and set classes on node polygons
    $dom->find('.node polygon:last-of-type')->each(sub {
        my $el = shift;
        delete $el->attr->{'fill'};
        delete $el->attr->{'stroke'};
        $el->attr(class => 'border');
    });
    $dom->find('.node polygon[fill="#fefefe"]')->each(sub {
        my $el = shift;
        delete $el->attr->{'fill'};
        delete $el->attr->{'stroke'};
        $el->attr(class => 'column-name');
    });
    $dom->find('.node polygon[fill="#dddfdd"]')->each(sub {
        my $el = shift;
        delete $el->attr->{'fill'};
        delete $el->attr->{'stroke'};
        $el->attr(class => 'table-name');
    });

    # Remove and set attributes on texts
    $dom->find('text')->each(sub {
        my $text = shift;
        delete $text->attr->{'font-family'};
        delete $text->attr->{'font-size'};
        $text->attr('data-is-primary' => 1) if delete $text->attr->{'font-weight'};
        $text->attr('data-is-foreign' => 1) if delete $text->attr->{'text-decoration'};
    });
    $dom->find('.node text:first-of-type')->each(sub {
        my $el = shift;
        delete $el->attr->{'fill'};
        $el->attr->{'class'} = 'table-name';
    });
    $dom->find('.node text:not(.table-name)')->each(sub {
        my $el = shift;
        delete $el->attr->{'fill'};
        $el->attr(class => 'column-name');
    });

    # Add data attributes to everything in nodes
    $dom->find('.node')->each(sub {
        my $node = shift;
        my $table_name = $node->at('text.table-name')->all_text;
        $node->attr('data-table-name', $table_name);


        $node->find('text.column-name')->each(sub {
            my $el = shift;
            $el->attr('data-column-name', $el->all_text);
            $el->previous->attr('data-column-name', $el->all_text); # background polygon

            my $column_info = $self->schema->resultset($table_name)->result_source->column_info($el->all_text);

            # this should instead remove all sub references
            delete $column_info->{'_inflate_info'};
            if(defined $column_info->{'default_value'}) {
                $column_info->{'default_value'} = ref $column_info->{'default_value'}
                                                ? ${ $column_info->{'default_value'} }
                                                : qq{'$column_info->{'default_value'}'}
                                                ;
            }
            $el->attr('data-column-info', encode_json($column_info));

        });
    });
    # There might be a tiny <polygon.table-name> on top of the real <polygon.table-name> (used for padding during graphviz creation)
    # We don't want it any more, we only want the last <polygon.table-name> in each .node
    $dom->find('.node')->each(sub {
        shift->find('polygon.table-name')->reverse->each(sub {
            $_[0]->remove if $_[1] > 1;
        });
    });
    # <polygon.table-name> leave a small gap to the .border that we don't want.
    # Since the arrows overlap the default .border[stroke-width] the .border x-points
    # are moved inwards by 0.5, thereby fixing both problems at once.
    #   There is also a slightly larger gap between <.node polygon.table-name>
    # and the top border created by a padding element removed above. Hence
    # <.polygon.table-name> gets their two top y-points set to the .border top y-point.
    #   And finally, there's a gap between <polygon.table-name>
    # and the first <polygon.column-name>, also due to padding during creation.
    # This is removed by setting <polygon.table-name> lower y-points to those of
    # the first <polygon.column-name>.
    $dom->find('.node polygon.border')->each(sub {
        my $border = shift;
        my $table_name_polygon = $border->parent->at('polygon.table-name');
        my $column_name_polygon = $border->parent->at('polygon.column-name');

        # Turn [points]  '6.5,-591.22 6.5,-662.22 158.5,-662.22 158.5,-591.22 6.5,-591.22'
        # into           [{ x => 6.5, y => -591.22 }, ... ]
        my $points_to_array = sub {
            +{ x => shift, y => shift };
        };
        my $border_points      = [map { $points_to_array->(split /,/) } split / /, $border->attr('points') ];
        my $table_name_points  = [map { $points_to_array->(split /,/) } split / /, $table_name_polygon->attr('points')];
        my $column_name_points = [map { $points_to_array->(split /,/) } split / /, $column_name_polygon->attr('points')];

        # 0: bottom left, 1: top left, 2: top right, 3: bottom right, 4: bottom left again
        for my $point (1..4) {
            $border_points->[$point]{'x'} += .5                               if any { $point == $_ } (0, 1, 4);
            $border_points->[$point]{'x'} -= .5                               if any { $point == $_ } (2, 3);

            $table_name_points->[$point]{'y'} = $column_name_points->[1]{'y'}  if any { $point == $_ } (0, 3, 4);
            $table_name_points->[$point]{'y'} = $border_points->[$point]{'y'}  if any { $point == $_ } (1, 2);
        }
        $border->attr(points => join ' ', map { "$_->{'x'},$_->{'y'}" } @{ $border_points });
        $table_name_polygon->attr(points => join ' ', map { "$_->{'x'},$_->{'y'}" } @{ $table_name_points });
    });

    # Make edges aware of what they are connecting
    $dom->find('.edge')->each(sub {
        my $edge = shift;
        my $title = $edge->at('title');

        if($title->text =~ m{ ^ ([^:]+) : ([^-]+?) -> ([^:;]+) : (.+) $ }x) {
            my $origin_table = $1;
            my $origin_column = $2;
            my $destination_table = $3;
            my $destination_column = $4;

            # Restore table names. See also node_name()
            $origin_table =~ s{__}{::}g;
            $destination_table =~ s{__}{::}g;
            $edge->attr('data-origin-table', $origin_table);
            $edge->attr('data-origin-column', $origin_column);
            $edge->attr('data-destination-table', $destination_table);
            $edge->attr('data-destination-column', $destination_column);
            $title->content("$origin_table.$origin_column -> $destination_table.$destination_column");
        }
    });
    # cleanup edges
    $dom->find('.edge path, .edge polygon, .edge polyline, .edge ellipse')->each(sub {
        my $el = shift;
        delete $el->attr->{'stroke'};
        delete $el->attr->{'stroke-width'};
        delete $el->attr->{'fill'};
    });
    # There are annoying gaps in edges if the edge is connected to the right side of a node
    # and the relation is a one-to-anything (in other words: the 'crow' arrow type works and
    # the 'tee' doesn't).
    # Replace the right point of the polyline with the first point of the path, after also
    # adding 0.5 to the x value to ensure overlap.
    $dom->find('.edge')->each(sub {
        my $edge = shift;
        my $line = $edge->children('title + path + polyline')->first;
        return if !defined $line;
        my $pathd = $line->previous->attr('d');

        # path: M168.679,-630.646C282.32,-605.396 149.41,-142.338...
        # line: 158.5,-631.72 163.472,-631.195
        return if $pathd !~ m{^M (?<x>[\d.-]+) , (?<y>[\d.-]+) }x;

        my @points = split m/\s/, $line->attr('points');
        $points[-1] = join ',' => ($+{'x'} + 0.5, $+{'y'});
        $line->attr(points => join ' ', @points);
    });


    # Fix the graph name
    my $schema_name = ref $self->schema;
    $dom->find('title')->first(content => $schema_name);

    my $rendered = $dom->to_string;
    $rendered =~ s{\n+}{\n}g;

    return $rendered;

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
            info => $rs->column_info($column),
        };
    }
    $self->graph->add_node(
        name => $node_name,
        label => $self->create_label_html($node_name, $label_data),
        margin => 0.01,
    );
}

sub add_edges {
    my $self = shift;
    my $source_name = shift;

    my $rs = $self->schema->resultset($source_name)->result_source;

    RELATION:
    for my $relation_name ($self->schema->resultset($source_name)->result_source->relationships) {
        my $node_name = $self->node_name($source_name);

        my $relation = $rs->relationship_info($relation_name);
        $relation->{'_name'} = $relation_name;
        (my $other_source_name = $relation->{'class'}) =~ s{^.*?::Result::}{};

        next RELATION if $self->has_skip_result_sources && any { $other_source_name eq $_ } @{ $self->skip_result_sources };
        my $other_node_name = $self->node_name($other_source_name);

        my $other_rs = $self->schema->resultset($other_source_name)->result_source;
        my $other_relation;

        OTHER_RELATION:
        for my $other_relation_name ($other_rs->relationships) {
            my $relation_to_attempt = $other_rs->relationship_info($other_relation_name);

            (my $possibly_original_class = $relation_to_attempt->{'class'}) =~ s{^.*?::Result::}{};
            next OTHER_RELATION if $possibly_original_class ne $source_name;

            # When we are rendering *part* of the schema, result_sources() doesn't contain all result sources in the schema, but only those we will display.
            # If the current relation points to a result source outside the wanted degrees_of_separation, we are not interested.
            next RELATION if $self->has_wanted_result_sources && none { $other_source_name eq $_ } @{ $self->result_sources };

            $other_relation = $relation_to_attempt;
            $other_relation->{'_name'} = $other_relation_name;
        }

        # Check for missing reverse relations, but only if we are displaying the whole schema.
        if(!$self->has_wanted_result_sources && !defined $other_relation) {
            $self->logger->info("! No reverse relationship $source_name <$relation_name> <-> $other_source_name");

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

        # Have we already added the edge from the reversed direction?
     #   say "       ! but not really " if exists $self->added_relationships->{"$other_node_name.$headport-->$node_name.$tailport"};
        next RELATION if exists $self->added_relationships->{"$other_node_name.$headport-->$node_name.$tailport"};


        # Since we are messing with source_name below, we need to not overwrite $source_name since that would
        # interfere with the next looping.
       # my $from_source_name = $source_name;

        # When displaying a single result source, and its closest relations, place some relations to the left and some to the right.
        # The placement is dependent on the direction of the connection. Hence: invert some relations.
        if(   $self->has_wanted_result_sources
           && any { $source_name eq $_ || $other_source_name eq $_} @{ $self->wanted_result_sources }
           && any { $_ eq $self->get_relation_type($relation) } (qw/belongs_to has_one/)) {

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
        $self->logger->debug("adds edge $node_name.$tailport -> $other_node_name.$headport");
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

        $self->added_relationships->{ "$node_name.$tailport-->$other_node_name.$headport" } = 1;
        $self->added_relationships->{ "$other_node_name.$headport-->$node_name.$tailport" } = 1;

    }
}

sub get_relation_type {
    my $self = shift;
    my $relation = shift;

    my $accessor = $relation->{'attrs'}{'accessor'};
    my $is_depends_on = $relation->{'attrs'}{'is_depends_on'};
    my $join_type = exists $relation->{'attrs'}{'join_type'} ? lc $relation->{'attrs'}{'join_type'} : 0;

    my $has_many   = $accessor eq 'multi'  && !$is_depends_on &&  $join_type eq 'left' ? 1 : 0;
    my $might_have = $accessor eq 'single' && !$is_depends_on &&  $join_type eq 'left' ? 1 : 0;
    my $belongs_to = $accessor eq 'single' &&  $is_depends_on && !$join_type           ? 1 : 0;
    my $has_one    = $accessor eq 'single' && !$is_depends_on && !$join_type           ? 1 : 0;

    return $has_many   ? 'has_many'
         : $belongs_to ? 'belongs_to'
         : $might_have ? 'might_have'
         : $has_one    ? 'has_one'
         :               'unknown'
         ;
}
sub get_arrow_type {
    my $self = shift;
    my $relation = shift;

    my $relation_type = $self->get_relation_type($relation);

    return $relation_type eq 'has_many'   ? join ('', qw/crow none odot/)
         : $relation_type eq 'belongs_to' ? join ('', qw/none tee/)
         : $relation_type eq 'might_have' ? join ('', qw/none tee none odot/)
         : $relation_type eq 'has_one'    ? join ('', qw/vee/)
         :                                  join ('', qw/dot dot dot/)
         ;
}
sub get_port_compass {
    my $self = shift;
    my $relation = shift;
    my $is_origin = shift;

    return '' if !$self->has_wanted_result_sources;
    my $relation_type = $self->get_relation_type($relation);

    return $relation_type eq 'has_many'   ? $is_origin ? ':e' : ':w'
         : $relation_type eq 'belongs_to' ? $is_origin ? ':w' : ':e'
         : $relation_type eq 'might_have' ? $is_origin ? ':w' : ':e'
         : $relation_type eq 'has_one'    ? $is_origin ? ':e' : ':w'
         :                                  ''
         ;
}

# Port names can't have semicolons in them.
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
            <tr><td align="left" port="$clean_column_name" bgcolor="#fefefe"> <font point-size="10" color="#222222">$column_name</font><font color="white">_@{[ $self->padding($column_name) ]}</font></td></tr>};
    }
    # Don't change colors here without fixing svg(). Magic numbers..
    my $html = qq{
        <<table cellborder="0" cellpadding="0.8" cellspacing="0" border="@{[ $mark_label ? '3' : '1' ]}" width="150">
            <tr><td bgcolor="#DDDFDD" width="150"><font point-size="2"> </font></td></tr>
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
