use strict;
use warnings;
use Test::More;
use if $ENV{'AUTHOR_TESTING'}, 'Test::Warnings';

use DBIx::Class::Visualizer;
ok 1, 'Loaded';

done_testing;
