#!/usr/bin/env perl

use strict;
use warnings;

my ($report_directory) = @ARGV;
die "usage: check-coverage.pl REPORT-DIRECTORY\n"
  unless defined $report_directory && length $report_directory;

opendir my $directory_handle, $report_directory
  or die "Cannot read coverage directory $report_directory: $!\n";
my @reports = sort grep { /\.html\z/ && -f "$report_directory/$_" }
  readdir $directory_handle;
closedir $directory_handle;

die "No HTML coverage reports found in $report_directory.\n"
  unless @reports;

my %allowed_default_fragments = (
  'src/http1-serialize.lisp' => {
    '(context :serialization)' => 1,
  },
  'http2/transport-core.lisp' => {
    '(max-frame-size +http2-default-max-frame-size+)' => 1,
  },
  'src/utilities.lisp' => {
    '(allow-list t)' => 1,
    '(context :text)' => 1,
  },
  'src/uri.lisp' => {
    '(scheme "http")' => 1,
    '(path "/")' => 1,
  },
);

sub relative_source_path {
  my ($reported_path) = @_;
  return "$1/$2" if $reported_path =~ m{/(src|http2)/(.+)\z};
  return $reported_path;
}

sub decode_fragment {
  my ($fragment) = @_;
  $fragment =~ s/&#([0-9]+);/chr($1)/ge;
  $fragment =~ s/\x{a0}/ /g;
  return $fragment;
}

sub allowed_fragment {
  my ($source_path, $line_number, $fragment) = @_;
  my $decoded = decode_fragment($fragment);
  return 1 if $line_number == 1 && $decoded =~ /\A\(in-package /;
  return $allowed_default_fragments{$source_path}{$decoded} // 0;
}

my $source_report_count = 0;
my $violation_count = 0;

for my $report (@reports) {
  my $report_path = "$report_directory/$report";
  open my $handle, '<', $report_path
    or die "Cannot read coverage report $report_path: $!\n";

  my $source_path;
  while (my $line = <$handle>) {
    if ($line =~ m{Coverage report:\s+(.+?)\s+<br}) {
      $source_path = relative_source_path($1);
      ++$source_report_count;
      next;
    }
    next unless $line =~ m{<div class='source'>};

    my ($line_number) = $line =~ m{<div class='line-number'><code>(\d+)</code>};
    my @unexecuted_fragments =
      $line =~ m{<span class='state-2'>(.*?)</span>}g;
    next unless @unexecuted_fragments;

    for my $fragment (@unexecuted_fragments) {
      next if defined $source_path
        && allowed_fragment($source_path, $line_number, $fragment);
      printf STDERR "%s:%s: uncovered expression %s\n",
        ($source_path // $report_path),
        ($line_number // '?'),
        decode_fragment($fragment);
      ++$violation_count;
    }
  }
  close $handle;
}

die "No source coverage reports found in $report_directory.\n"
  unless $source_report_count;
die "$violation_count uncovered expressions remain in the coverage report.\n"
  if $violation_count;

print "Coverage expression audit passed: $source_report_count source reports checked.\n";
