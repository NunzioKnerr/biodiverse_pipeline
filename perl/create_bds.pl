#!/usr/bin/perl -w
use strict;
use warnings;
use Biodiverse::BaseData;
use Biodiverse::Common;
use Biodiverse::ElementProperties;

use Data::Dumper qw/Dumper/;

use Getopt::Long::Descriptive;

$| = 1;

my ($opt, $usage) = describe_options(
  '%c <arguments>',
  [ 'csv_file=s',   'The input csv file', { required => 1 } ],
  [ 'out_file|output_bd=s',  'The output basedata file', { required => 1 }],
  [ 'label_column_number:i',    'Column containing the label name [default= "0"]', { default => 0 } ],
  [ 'group_column_number_x:i',  'Column containing the x-axis values [default= "1"]', { default => 1 } ],
  [ 'group_column_number_y:i',  'Column containing the y-axis values [default= "2"]', { default => 2 } ],
  [ 'cell_size_x:f',  'Cell size of x-axis [default= "100000"]', { default => 100000 } ],
  [ 'cell_size_y:f',  'Cell size of y-axis [default= "100000"]', { default => 100000 } ],
  [],
  [ 'help',       "print usage message and exit" ],
);

 
if ($opt->help) {
    print($usage->text);
    exit;
}

my $csv_file            = $opt->csv_file;
my $out_file            = $opt->out_file;
my $label_column_number = $opt->label_column_number;
my $group_column_number_x = $opt->group_column_number_x;
my $group_column_number_y = $opt->group_column_number_y;
my $cell_size_x = $opt->cell_size_x;
my $cell_size_y = $opt->cell_size_y;
#my $remap_file        = $opt->remap_file;
#my $input_cols_str    = $opt->input_cols;
#my $remapped_cols_str = $opt->remapped_cols;
#my $input_sep_char    = $opt->input_sep_char;
#my $input_quote_char  = $opt->input_quote_char;
#my $verbose           = $opt->verbose;

#chdir $input_directory;

#my $out_file = $output_bds_file;# . $reserve;
#my $out_dir = $output_directory;
#
#if (! eval {chdir $out_dir}) {
#    mkdir $out_dir;
#    chdir $out_dir;
#};


my @table;

my $double_quotes = q{"};

my $dist = 'all';

my @names = (
    #'../final_hornworts_species_actual_samples.csv',
	$csv_file,
);

my %input_files = (
    $names[0] => {
        input_quotes    => $double_quotes,
        label_columns   => [$label_column_number],
        group_columns   => [$group_column_number_x, $group_column_number_y],
    }
);

#~ my %lb_remap_data = (
    #~ file                  => $remap_csv_file,
    #~ input_element_cols    => [$input_element_cols],
    #~ remapped_element_cols => [$remapped_element_cols],
    #~ range                 => 4,
    #~ exclude_cols          => [5],
    #~ input_quote_char      => $double_quotes,
#~ );


my $bd;
eval {
    $bd = Biodiverse::BaseData -> new (file => $out_file . '.bds');
};
if (not defined $bd) {
    $bd = create_bd();
    $bd -> save;
}


sub create_bd {
    my $bd = Biodiverse::BaseData -> new (
        NAME        => $out_file,
        CELL_SIZES  => [$cell_size_x, $cell_size_y],
    );

    #~ my $lb_remap = Biodiverse::ElementProperties -> new ();
    #~ $lb_remap -> import_data (%lb_remap_data);
    #~ $lb_remap -> export (
        #~ format => 'Delimited text',
        #~ list   => 'PROPERTIES',
        #~ file   => 'label_rmp.csv',
    #~ );

    #print Dumper ($bd -> dump_to_yaml (data => $$lb_remap{remap}));

    foreach my $file (@names) {
        my %args = %{$input_files{$file}};
        $bd -> load_data (
            input_sep_char        => ",",
            include_columns       => [],
            %args,
            input_quotes          => $double_quotes,
            input_files           => [$file],
            use_label_properties  => 0,
            #label_properties      => $lb_remap,
        );
    }

    return $bd;
}

