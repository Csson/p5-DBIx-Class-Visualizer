# NAME

DBIx::Class::Visualizer - Visualize a DBIx::Class schema

<div>
    <p>
    <img src="https://img.shields.io/badge/perl-5.10.1+-blue.svg" alt="Requires Perl 5.10.1+" />
    <a href="https://travis-ci.org/Csson/p5-DBIx-Class-Visualizer"><img src="https://api.travis-ci.org/Csson/p5-DBIx-Class-Visualizer.svg?branch=master" alt="Travis status" /></a>
    <a href="http://cpants.cpanauthors.org/release/CSSON/DBIx-Class-Visualizer-0.0100"><img src="http://badgedepot.code301.com/badge/kwalitee/CSSON/DBIx-Class-Visualizer/0.0100" alt="Distribution kwalitee" /></a>
    <a href="http://matrix.cpantesters.org/?dist=DBIx-Class-Visualizer%200.0100"><img src="http://badgedepot.code301.com/badge/cpantesters/DBIx-Class-Visualizer/0.0100" alt="CPAN Testers result" /></a>
    <img src="https://img.shields.io/badge/coverage-15.3%-red.svg" alt="coverage 15.3%" />
    </p>
</div>

# VERSION

Version 0.0100, released 2016-09-04.

# SYNOPSIS

    use DBIx::Class::Visualizer;
    use A::DBIx::Class::Schema;

    my $schema = A::DBIx::Class::Schema->connect;
    my $svg = DBIx::Class::Visualizer->new->svg;

# DESCRIPTION

DBIx::Class::Visualizer is a [GraphViz2](https://metacpan.org/pod/GraphViz2) renderer for [DBIx::Class](https://metacpan.org/pod/DBIx::Class) schemata.

On the relatively small schemata (about twenty result classes) that I have tried it on it produces reasonably readable graphs. See `example/visualized.svg` for a
simple example (also available on [Github](http://htmlpreview.github.io/?https://github.com/Csson/p5-DBIx-Class-Visualizer/blob/master/example/visualized.svg)).

# ATTRIBUTES

## schema

Required instance of a [DBIx::Class::Schema](https://metacpan.org/pod/DBIx::Class::Schema).

## graphviz\_config

Optional hashref. This hashref is passed to the [GraphViz2](https://metacpan.org/pod/GraphViz2) constructor. Set this if the defaults don't work. Setting this will replace the defaults.

## graph

Can't be passed in the constructor. This contains the constructed [GraphViz2](https://metacpan.org/pod/GraphViz2) object. Use this if you wish to render the visualization manually:

    my $png = DBIx::Class::Visualizer->new(schema => $schema)->graph->run(output_file => 'myschema.png', format => 'png');

# METHODS

## new

The constructor.

## svg

Takes no arguments, and returns the rendered svg document as a string.

# SEE ALSO

- [Mojolicious::Plugin::DbicSchemaViewer](https://metacpan.org/pod/Mojolicious::Plugin::DbicSchemaViewer) - A [Mojolicious](https://metacpan.org/pod/Mojolicious) plugin that uses this class
- [GraphViz2::DBI](https://metacpan.org/pod/GraphViz2::DBI) - A similar idea

# SOURCE

[https://github.com/Csson/p5-DBIx-Class-Visualizer](https://github.com/Csson/p5-DBIx-Class-Visualizer)

# HOMEPAGE

[https://metacpan.org/release/DBIx-Class-Visualizer](https://metacpan.org/release/DBIx-Class-Visualizer)

# AUTHOR

Erik Carlsson <info@code301.com>

# COPYRIGHT AND LICENSE

This software is copyright (c) 2016 by Erik Carlsson.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.
