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
  return "$1/$2" if $reported_path =~ m{/(src|http2|client|http3)/(.+)\z};
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
my $branch_violation_count = 0;
my %source_summaries;
my @non_source_reports;
my $index_html;

for my $report (@reports) {
  my $report_path = "$report_directory/$report";
  open my $handle, '<', $report_path
    or die "Cannot read coverage report $report_path: $!\n";

  my $html = do { local $/; <$handle> // '' };
  close $handle;
  if ($report eq 'cover-index.html') {
    $index_html = $html;
    next;
  }

  my @source_paths = $html =~ m{Coverage report:\s+(.+?)\s+<br}g;
  unless (@source_paths) {
    push @non_source_reports, $report;
    next;
  }
  die "Duplicate source heading in $report_path.\n" if @source_paths != 1;
  my $source_path = relative_source_path($source_paths[0]);
  ++$source_report_count;
  my %summary;
  while ($html =~ m{<td>(expression|branch)</td>(.*?)</tr>}sg) {
    my ($kind, $cells) = ($1, $2);
    die "Duplicate $kind summary in $report_path.\n"
      if exists $summary{$kind};
    my ($covered, $total) = $cells =~
      m{\A<td>([0-9]+)</td><td>([0-9]+)</td><td>[^<>]*</td>\z};
    die "Invalid $kind summary in $report_path.\n"
      unless defined $covered;
    $summary{$kind} = [$covered, $total];
  }

  my $source_markers = () = $html =~ m{<div class='source'>}g;
  my $source_rows = 0;
  my $nonempty_body = 0;
  while ($html =~ m{<div class='source'><div class='line-number'><code>(.*?)</code></div><code>(.*?</code>)</div>}sg) {
    my ($line_number, $body) = ($1, $2);
    die "Invalid source line number in $report_path.\n"
      unless $line_number =~ /\A[1-9][0-9]*\z/;
    ++$source_rows;
    my $text = $body;
    $text =~ s/<[^>]*>//g;
    $nonempty_body ||= decode_fragment($text) =~ /\S/;

    # States 6 and 9 encode opposite single-branch outcomes; 10 encodes neither.
    while ($body =~ m{<span class='state-(6|9|10)'>(.*?)(?:</span>|</code>)}sg) {
      printf STDERR "%s:%s: uncovered branch state-%s %s\n",
        $source_path, $line_number, $1, decode_fragment($2);
      ++$branch_violation_count;
    }
    # SBCL can leave the final coverage span open until the source code ends.
    my @unexecuted_fragments =
      $body =~ m{<span class='state-2'>(.*?)(?:</span>|</code>)}sg;
    next unless @unexecuted_fragments;

    for my $fragment (@unexecuted_fragments) {
      next if allowed_fragment($source_path, $line_number, $fragment);
      printf STDERR "%s:%s: uncovered expression %s\n",
        $source_path,
        $line_number,
        decode_fragment($fragment);
      ++$violation_count;
    }
  }
  die "Missing or malformed source rows in $report_path.\n"
    unless $source_rows && $source_rows == $source_markers;
  die "Empty source body in $report_path.\n" unless $nonempty_body;
  for my $kind ('expression', 'branch') {
    die "Missing $kind summary in $report_path.\n"
      unless exists $summary{$kind};
    my ($covered, $total) = @{$summary{$kind}};
    die "$source_path: $kind coverage is $covered/$total, not complete.\n"
      unless $covered == $total;
  }
  $source_summaries{$report} = \%summary;
}

die "No source coverage reports found in $report_directory.\n"
  unless $source_report_count;
die "Missing source heading in report $non_source_reports[0].\n"
  if @non_source_reports;

if (defined $index_html) {
  my %index_entries;
  my $row_markers = () = $index_html =~ /<tr\b/g;
  my $rows = 0;
  while ($index_html =~ m{(<tr\b[^>]*>.*?</tr>)}sg) {
    my $row = $1;
    ++$rows;
    # These are SBCL's generated headings, not source coverage entries.
    next if $row =~ m{\A<tr class='head-row'><td></td><td class='main-head' colspan='3'>Expression</td><td class='main-head' colspan='3'>Branch</td></tr>\z};
    next if $row =~ m{\A<tr class='head-row'><td width='80px'>Source file</td>(?:<td width='80px'>Covered</td><td width='80px'>Total</td><td width='80px'>%</td>){2}</tr>\z};
    next if $row =~ m{\A<tr class='subheading'><td colspan='7'>[^<>]+</td></tr>\z};
    my ($href, $covered, $total, $branch_covered, $branch_total) = $row =~
      m{\A<tr(?: class='(?:odd|even)')?><td class='text-cell'><a href='([^']+)'>[^<>]+</a></td><td>([0-9]+)</td><td>([0-9]+)</td><td>[^<>]*</td><td>([0-9]+)</td><td>([0-9]+)</td><td>[^<>]*</td></tr>\z};
    die "Invalid coverage index row.\n" unless defined $href;
    die "Duplicate coverage index entry $href.\n" if exists $index_entries{$href};
    die "Unmatched coverage index entry $href.\n" unless exists $source_summaries{$href};
    $index_entries{$href} = 1;
    my %counts = (expression => [$covered, $total], branch => [$branch_covered, $branch_total]);
    for my $kind ('expression', 'branch') {
      my ($index_covered, $index_total) = @{$counts{$kind}};
      my ($source_covered, $source_total) = @{$source_summaries{$href}{$kind}};
      die "Coverage index $href: $kind summary mismatch ($index_covered/$index_total versus $source_covered/$source_total).\n"
        unless $index_covered == $source_covered && $index_total == $source_total;
      die "Coverage index $href: $kind coverage is $index_covered/$index_total, not complete.\n"
        unless $index_covered == $index_total;
    }
  }
  die "Empty or malformed coverage index.\n"
    unless $rows && $rows == $row_markers && keys %index_entries;
  for my $report (sort keys %source_summaries) {
    die "Missing coverage index entry $report.\n" unless exists $index_entries{$report};
  }
}

die "$violation_count uncovered expressions remain in the coverage report.\n"
  if $violation_count;
die "$branch_violation_count uncovered branches remain in the coverage report.\n"
  if $branch_violation_count;

print "Coverage expression audit passed: $source_report_count source reports checked.\n";
