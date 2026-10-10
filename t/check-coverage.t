use strict;
use warnings;
use File::Spec;
use File::Temp qw(tempdir);
use FindBin;
use IPC::Open3;
use Symbol qw(gensym);
use Test::More;

my $checker = File::Spec->catfile($FindBin::Bin, '..', 'scripts', 'check-coverage.pl');

sub audit_reports {
  my (%reports) = @_;
  my $directory = tempdir(CLEANUP => 1);
  for my $name (sort keys %reports) {
    open my $file, '>', File::Spec->catfile($directory, $name) or die $!;
    print {$file} $reports{$name};
    close $file or die $!;
  }
  my $stderr = gensym;
  my $pid = open3(undef, my $stdout, $stderr, $^X, $checker, $directory);
  my $output = do { local $/; <$stdout> // '' };
  my $errors = do { local $/; <$stderr> // '' };
  waitpid($pid, 0);
  return ($?, $output, $errors);
}

sub source_report {
  my ($path, $line_number, $spans) = @_;
  return "<h3>Coverage report: $path <br />\n"
    . "<table class='summary'>"
    . "<tr><td>expression</td><td>1</td><td>1</td><td>100.0</td></tr>\n"
    . "<tr><td>branch</td><td>0</td><td>0</td><td>0.0</td></tr></table>\n"
    . "<nobr><div class='source'><div class='line-number'><code>$line_number</code></div>"
    . "<code>$spans</code></div></body></html>\n";
}

sub index_report {
  my (@entries) = @_;
  my $html = "<table class='summary'>"
    . "<tr class='head-row'><td></td><td class='main-head' colspan='3'>Expression</td>"
    . "<td class='main-head' colspan='3'>Branch</td></tr>"
    . "<tr class='head-row'><td width='80px'>Source file</td>"
    . "<td width='80px'>Covered</td><td width='80px'>Total</td><td width='80px'>%</td>"
    . "<td width='80px'>Covered</td><td width='80px'>Total</td><td width='80px'>%</td></tr>"
    . "<tr class='subheading'><td colspan='7'>/project/src/</td></tr>";
  for my $entry (@entries) {
    my ($href, $covered, $total, $branch_covered, $branch_total) = @$entry;
    $html .= "<tr class='odd'><td class='text-cell'><a href='$href'>fixture.lisp</a></td>"
      . "<td>$covered</td><td>$total</td><td>100.0</td>"
      . "<td>$branch_covered</td><td>$branch_total</td><td>-</td></tr>";
  }
  return "$html</table>";
}

# SBCL 2.6.6 source rows, with a public pathname and the generated summaries.
sub native_source_report {
  return <<'HTML';
<h3>Coverage report: /project/src/fixture.lisp <br />
<table class='summary'><tr><td>expression</td><td>7</td><td>7</td><td>100.0</td></tr>
<tr><td>branch</td><td>2</td><td>2</td><td>100.0</td></tr></table>
<div class='state-6'>One branch taken</div><div class='state-10'>Neither branch taken</div>
<nobr><div class='source'><div class='line-number'><code>1</code></div><code>&#160;<span class='state-1'>&#40;in&#45;package&#160;&#35;&#58;cl&#45;user&#41;</span><span class='state-0'></span></code></div></nobr>
<nobr><div class='source'><div class='line-number'><code>2</code></div><code>&#160;<span class='state-1'>&#40;declaim&#160;&#40;optimize&#160;&#40;sb&#45;cover&#58;store&#45;coverage&#45;data&#160;3&#41;&#41;&#41;</span><span class='state-0'></span></code></div></nobr>
<nobr><div class='source'><div class='line-number'><code>3</code></div><code>&#160;<span class='state-1'>&#40;defun&#160;independent&#45;driver&#45;probe&#160;&#40;&#38;key&#160;&#40;value&#160;7&#41;&#41;</span><span class='state-0'></span></code></div></nobr>
<nobr><div class='source'><div class='line-number'><code>4</code></div><code>&#160;<span class='state-1'>&#160;&#160;&#40;if&#160;</span><span class='state-5'>&#40;evenp&#160;value&#41;</span><span class='state-1'>&#160;&#40;1&#43;&#160;value&#41;&#160;&#40;1&#45;&#160;value&#41;&#41;&#41;</code></div></body></html>
HTML
}

sub rejects_report {
  my ($label, $diagnostic, %reports) = @_;
  my ($status, $output, $errors) = audit_reports(%reports);
  isnt($status, 0, $label);
  is($output, '', "$label does not report success");
  like($errors, $diagnostic, "$label identifies the fault");
}

{
  my ($status, $output, $errors) = audit_reports(
    'source.html' => source_report('/project/client/form.lisp', 2,
                                   "<span class='state-1'>&#40;identity&#160;1&#41;</span>"),
    'cover-index.html' => index_report(['source.html', 1, 1, 0, 0]),
  );
  is($status, 0, 'executed source passes');
  like($output, qr/1 source reports checked/, 'index does not count as a source report');
  is($errors, '', 'executed source has no diagnostics');
}

for my $ending ('</span>', '') {
  my ($status, $output, $errors) = audit_reports(
    'source.html' => source_report('/project/src/example.lisp', 78,
                                   "<span class='state-2'>&#40;missing&#160;1&#41;$ending"),
  );
  isnt($status, 0, "unexecuted source fails with span ending '$ending'");
  is($output, '', 'failed audit does not report success');
  like($errors, qr/src\/example\.lisp:78: uncovered expression \(missing 1\)/,
       'unexecuted expression is decoded and located');
  like($errors, qr/1 uncovered expressions remain/, 'last source line is counted');
}

for my $directory ('src', 'http2', 'client', 'http3') {
  my (undef, undef, $errors) = audit_reports(
    'source.html' => source_report("/project/$directory/example.lisp", 2,
                                   "<span class='state-2'>unexecuted</span>"),
  );
  like($errors, qr/^\Q$directory\E\/example\.lisp:2:/m,
       "$directory source diagnostic is repository-relative");
}

for my $ending ('</span>', '') {
  my ($status, undef, $errors) = audit_reports(
    'source.html' => source_report('/project/src/uri.lisp', 2,
                                   "<span class='state-2'>&#40;path&#160;&#34;&#47;&#34;&#41;$ending"),
  );
  is($status, 0, 'existing exact default exception remains allowed');
  is($errors, '', 'allowed default has no diagnostics');
}

{
  my ($status, undef, $errors) = audit_reports(
    'source.html' => source_report('/project/src/uri.lisp', 2,
      "<span class='state-2'>&#40;path&#160;&#34;&#47;&#34;&#41;</span>"
      . "<span class='state-1'> </span><span class='state-2'>&#40;missing&#41;"),
  );
  isnt($status, 0, 'allowed fragment does not hide adjacent unexecuted fragment');
  like($errors, qr/uncovered expression \(missing\)/, 'adjacent fragment is reported');
  like($errors, qr/1 uncovered expressions remain/, 'only the violation is counted');
}

{
  my ($status, undef, $errors) = audit_reports(
    'source.html' => source_report('/project/src/example.lisp', 2,
      "<span class='state-2'>missing</code><code>"
      . "<span class='state-1'>executed-token</span>"),
  );
  isnt($status, 0, 'unclosed fragment fails before a later code block');
  like($errors, qr/uncovered expression missing\n/,
       'unclosed fragment stops at its own code boundary');
  unlike($errors, qr/executed-token/, 'later code block is not part of the diagnostic');
}

{
  my ($status, undef, $errors) = audit_reports(
    'source.html' => source_report('/project/client/example.lisp', 1,
      "<span class='state-2'>&#40;in&#45;package&#160;&#35;&#58;example&#41;"),
  );
  is($status, 0, 'first-line package declaration remains allowed');
  is($errors, '', 'allowed declaration has no diagnostics');
}

{
  my ($status, undef, $errors) = audit_reports();
  isnt($status, 0, 'empty report directory fails');
  like($errors, qr/No HTML coverage reports found/, 'empty directory is distinguished');
}

{
  my ($status, undef, $errors) = audit_reports('cover-index.html' => '<html>Index</html>');
  isnt($status, 0, 'index-only report fails');
  like($errors, qr/No source coverage reports found/, 'index cannot establish source coverage');
}

for my $kind ('expression', 'branch') {
  my $report = source_report('/project/src/example.lisp', 2,
                            "<span class='state-1'>executed</span>");
  $report =~ s{<td>$kind</td><td>[0-9]+</td><td>[0-9]+</td>}
              {<td>$kind</td><td>9999</td><td>10000</td>};
  my ($status, $output, $errors) = audit_reports('source.html' => $report);
  isnt($status, 0, "$kind integer counts reject rounded 100 percent");
  is($output, '', 'incomplete summary does not report success');
  like($errors, qr/\Q$kind coverage is 9999\/10000, not complete\E/,
       'incomplete summary reports exact counts');
}

for my $kind ('expression', 'branch') {
  for my $fault ('missing', 'duplicate', 'invalid') {
    my $report = source_report('/project/src/example.lisp', 2,
                              "<span class='state-1'>executed</span>");
    my $row = qr{<tr><td>$kind</td><td>[0-9]+</td><td>[0-9]+</td><td>[^<>]*</td></tr>};
    if ($fault eq 'missing') {
      $report =~ s/$row//;
    } elsif ($fault eq 'duplicate') {
      $report =~ s/($row)/$1$1/;
    } else {
      $report =~ s{<td>$kind</td><td>[0-9]+</td>}{<td>$kind</td><td>bad</td>};
    }
    my ($status, $output, $errors) = audit_reports('source.html' => $report);
    isnt($status, 0, "$fault $kind summary fails");
    is($output, '', 'malformed summary does not report success');
    like($errors, qr/\Q$kind summary\E/, 'summary fault is identified');
  }
}

{
  my ($status, $output, $errors) = audit_reports(
    'source.html' => native_source_report(),
    'cover-index.html' => index_report(['source.html', 7, 7, 2, 2]),
  );
  is($status, 0, 'nonempty native source and generated index metadata pass');
  like($output, qr/1 source reports checked/, 'native report selects one source');
  is($errors, '', 'native report has no diagnostics');
}

for my $state (6, 9, 10) {
  for my $ending ('</span>', '') {
    my $report = native_source_report();
    $report =~ s{<span class='state-5'>(.*?)</span>}
                {<span class='state-$state'>$1$ending};
    rejects_report("native branch state-$state with ending '$ending' fails",
      qr/src\/fixture\.lisp:4: uncovered branch state-$state/,
      'source.html' => $report,
      'cover-index.html' => index_report(['source.html', 7, 7, 2, 2]));
  }
}

for my $fault ('missing', 'empty', 'whitespace', 'line-only', 'unclosed-code', 'invalid-line', 'zero-line') {
  my $report = native_source_report();
  if ($fault eq 'missing') {
    $report =~ s{<nobr><div class='source'>.*?(?=<nobr><div class='source'>|\z)}{}sg;
  } elsif ($fault eq 'unclosed-code') {
    $report =~ s{</code></div>}{</div>}g;
  } elsif ($fault eq 'invalid-line' || $fault eq 'zero-line') {
    my $value = $fault eq 'zero-line' ? '0' : 'bad';
    $report =~ s{(<div class='line-number'><code>)[0-9]+}{$1$value};
  } else {
    my $body = $fault eq 'whitespace' ? '&#160;' : '';
    $report =~ s{(<div class='source'><div class='line-number'><code>[0-9]+</code></div>)<code>.*?</div>}
                {$1<code>$body</code></div>}sg;
    $report =~ s{</div><code></code></div>}{</div></div>}g if $fault eq 'line-only';
  }
  rejects_report("$fault source input fails", qr/source|line|body/i,
    'source.html' => $report);
}

{
  my $source = native_source_report();
  $source =~ s{</body>}{<nobr><div class='source'><div class='line-number'><code>5</code></div><code>&#160;</code></div></nobr></body>};
  my $index = index_report(['first.html', 7, 7, 2, 2], ['second.html', 7, 7, 2, 2]);
  $index =~ s{(<tr class=)'odd'}{$1'even'};
  my ($status, $output, $errors) = audit_reports(
    'first.html' => $source, 'second.html' => native_source_report(),
    'cover-index.html' => $index);
  is($status, 0, 'multiple generated index rows and a blank source line pass');
  like($output, qr/2 source reports checked/, 'all indexed sources are selected');
  is($errors, '', 'blank line and even index row have no diagnostics');
}

{
  my $report = native_source_report();
  $report =~ s{class='state-1'}{class='state-2'}g;
  rejects_report('genuine uncovered body with complete summaries fails',
    qr/uncovered expression/,
    'source.html' => $report,
    'cover-index.html' => index_report(['source.html', 7, 7, 2, 2]));
}

for my $counts ([6, 7, 2, 2], [8, 8, 2, 2], [7, 7, 1, 2], [7, 7, 4, 4]) {
  rejects_report("index counts @$counts inconsistent with native summary fail",
    qr/index.*(?:coverage|summary|mismatch)/i,
    'source.html' => native_source_report(),
    'cover-index.html' => index_report(['source.html', @$counts]));
}

for my $fault ('empty', 'missing', 'duplicate', 'invalid', 'unmatched', 'extra', 'malformed') {
  my $index = index_report(['source.html', 7, 7, 2, 2]);
  if ($fault eq 'empty') {
    $index = '';
  } elsif ($fault eq 'missing') {
    $index = index_report();
  } elsif ($fault eq 'duplicate') {
    $index = index_report(['source.html', 7, 7, 2, 2], ['source.html', 7, 7, 2, 2]);
  } elsif ($fault eq 'invalid') {
    $index =~ s{<td>7</td>}{<td>bad</td>};
  } elsif ($fault eq 'unmatched') {
    $index =~ s/source\.html/missing.html/;
  } elsif ($fault eq 'extra') {
    $index = index_report(['source.html', 7, 7, 2, 2], ['extra.html', 1, 1, 0, 0]);
  } else {
    $index =~ s{<td>7</td>}{};
  }
  rejects_report("$fault index input fails", qr/index/i,
    'source.html' => native_source_report(), 'cover-index.html' => $index);
}

{
  rejects_report('unindexed source report fails', qr/index/i,
    'source.html' => native_source_report(), 'second.html' => native_source_report(),
    'cover-index.html' => index_report(['source.html', 7, 7, 2, 2]));
  rejects_report('non-source HTML cannot hide among source reports', qr/source|report/i,
    'source.html' => native_source_report(), 'empty.html' => '');
  my $report = native_source_report();
  $report =~ s{(<h3>Coverage report:.*?<br />)}{$1$1};
  rejects_report('duplicate source heading fails', qr/source|heading/i,
    'source.html' => $report);
}

done_testing();
