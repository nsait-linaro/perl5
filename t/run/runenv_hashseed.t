#!./perl
#
# Tests for PERL_PERMUTE_KEYS and PERL_HASH_SEED
#
# For PERL_PERMUTE_KEYS=NO or PERL_PERMUTE_KEYS=DETERMINISTIC we should
# get the same key order each time.
#
# For PERL_PREMUTE_KEYS=RANDOM we should get a different key order each
# time, or at least most times.
#
# For PERL_PERMUTE_KEYS=RANDOM the test is probabilistic and there is a
# 1/131072 chance per repeat that we might get the same key order.
#
# For all the test modes there is also a chance that we might get the same
# key order when we munge the hash seed, albeit a much lower chance.
#
# We do 50 repeats, so we have 50/131072 chance of getting the same
# results for the probabilistic tests, which we round up to 1 since the
# count of test fails can not be a fraction. So we allow 1 try per batch
# of repeats to produce the same result before we fail the test.

BEGIN {
    chdir 't' if -d 't';
    @INC = '../lib';
    require './test.pl';
    require Config;
    Config->import;
}

skip_all_without_config('d_fork');
skip_all("NO_PERL_HASH_ENV or NO_PERL_HASH_SEED_DEBUG set")
    if $Config{ccflags} =~ /-DNO_PERL_HASH_ENV\b/
    || $Config{ccflags} =~ /-DNO_PERL_HASH_SEED_DEBUG\b/;
use strict;
use warnings;

# enable DEBUG_RUNENV if you want to see what is being returned
# by the executed perl.
sub my_runperl_and_capture {
    my ($opts_hash, $cmd_array) = @_;
    my ( $out, $err )
        = runperl_and_capture( $opts_hash, $cmd_array );
    my $err_got_data = "";
    while ($err=~s/^(Got.*\n)//) {
        $err_got_data .= $1;
    }
    my @rand_bits;
    while ($err=~s/^(PL_hash_rand_bits=.*)\n//m) {
        push @rand_bits, $1;
    }
    $err =~ /HASH_SEED = (0x[a-f0-9]+)/
        or die "Failed to extract hash seed from runperl_and_capture";
    my $seed = $1;
    return ($out, $err, $seed, $err_got_data, \@rand_bits);
}

my @modes = (
        'NO',
        'RANDOM',
        'DETERMINISTIC'
    ); # 0, 1 and 2 respectively

my $repeat = 50; # if this changes adjust the comments below.
my $min_buckets = 100_000;
my $actual_buckets = 1;
$actual_buckets *= 2 while $actual_buckets <= $min_buckets;
my $key_expr = '"A" .. "Z", "a" .. "z", 0 .. 9';
my @keys = eval $key_expr;
my $allowed_fails = 2; # FIXME: compute from @keys and $actual_buckets

plan tests => (5 * $repeat)     # DETERMINISTIC
            + (2 * $repeat)     # NO
            + (1 * $repeat) + 1 # RANDOM mode
            + @modes;           # all modes

# Test that PERL_PERTURB_KEYS works as expected. We check that we get
# the same results if we use PERL_PERTURB_KEYS = 0 or 2 and we reuse the
# seed from previous run.
#
# Note the keys(%h) = $n will cause perl to allocate the power of 2 larger
# than $n buckets, so if $n = 100_000, then $actual_buckets will be 131072.
my $print_keys = [
    '-Dh',      # get extra debugging output on a DEBUGGING perl
    '-I../lib',
    (is_miniperl() ? () # no Hash::Util here!
                   : '-MHash::Util=hash_traversal_mask,num_buckets'),
    '-e',
    'my %h; keys(%h)=' . $min_buckets . '; ' .
    '@h{' . $key_expr . '}=(); @k=keys %h; ' .
      'print join ":", 0+@k, ' .
      (is_miniperl() ? '' :  # no Hash::Util here!
          'num_buckets(%h),hash_traversal_mask(\\%h), ') .
      'join ",", @k;'
];

for my $mode (@modes) {
    my $base_opts = {
        PERL_PERTURB_KEYS => $mode,
        PERL_HASH_SEED_DEBUG => 1,  # needed for non DEBUGGING builds
    };
    my $descr_mode = sprintf "PERL_PERTURB_KEYS = %s", $mode;
    my $random_same = 0;
    my $seed_change_same = 0;
    for my $try (1 .. $repeat) {
        my $descr = sprintf "%s, try %2d:", $descr_mode, $try;
        my ( $out, $err, $seed )
            = my_runperl_and_capture( $base_opts, $print_keys );
        my $run_opts = { %$base_opts, PERL_HASH_SEED => $seed };

        {
            # first test that if we munge the seed we get a
            # different result
            my $munged_seed = $seed;
            substr($munged_seed,-1)=~tr/0-9a-f/1-9a-f0/;
            if ($munged_seed eq $seed) {
                die "Failed to munge seed '$seed'";
            }

            local $run_opts->{PERL_HASH_SEED}= $munged_seed;
            my ( $new_out, $new_err, $new_seed )
                = my_runperl_and_capture( $run_opts, $print_keys );
            if ($new_seed ne $munged_seed) {
                die "Seed change didn't seem to propagate";
            }
            if ($out eq $new_out) {
                # there is at least a 1/131072 chance that we got the
                # same result.
                $seed_change_same++;
            }
        }

        # now we have to run it again.
        my ( $out1, $err1, $seed1, $err_got_data1, $rand_bits1 )
            = my_runperl_and_capture( $run_opts, $print_keys );

        # and once more, these two should be the same
        my ( $out2, $err2, $seed2, $err_got_data2, $rand_bits2 )
            = my_runperl_and_capture( $run_opts, $print_keys );

        is( $err, $err2,
            "$descr debug output was consistent between runs"
        );
        if( $seed1 ne $seed) {
            die "Seed wasn't consistent on retry 1";
        }
        if( $seed2 ne $seed) {
            die "Seed wasn't consistent on retry 2";
        }
        if ( $mode eq 'RANDOM' ) {
            # The result should be different most times, but there is at
            # least as 1/131072 chance that we got the same result.
            $random_same++ if $out1 eq $out2;
            next;
        }

        # From this point on we are testing DETERMINISTIC and NO only.

        is( $out1, $out2,
            "$descr results in the same key order each time"
        );

        next if $mode eq "NO";

        SKIP: {
            # skip these tests if we are not running in a DEBUGGING perl.
            skip "$descr not testing rand bits, not a DEBUGGING perl", 3
                if @$rand_bits1 + @$rand_bits2 == 0;

            is ( 0+@$rand_bits1, 0+@$rand_bits2,
                "$descr same count of rand_bits entries each time");

            my $max_i = $#$rand_bits1 > $#$rand_bits2
                      ? $#$rand_bits1 : $#$rand_bits2;

            my $bad_idx;
            for my $i (0 .. $max_i) {
                if (($rand_bits2->[$i] // "") ne
                    ($rand_bits1->[$i] // ""))
                {
                    $bad_idx = $i;
                    last;
                }
            }
            is($bad_idx, undef,
                "$descr bad rand bits data index should be undef");
            if (defined $bad_idx) {
                # we use is() to see the differing data, but this test
                # is expected will fail - the description seems a little
                # odd here, but since it will always fail it makes sense
                # in context.
                is($rand_bits2->[$bad_idx],$rand_bits1->[$bad_idx],
                    "$descr rand bits data is same at idx $bad_idx");
            } else {
                pass("$descr rand bits data does not differ");
            }
        }
    }

    if ($mode eq "RANDOM") {
        # There is a small chance we got the same result a few times
        # even when everything is working as expected. So allow a
        # certain number of fails.
        ok( $random_same <= $allowed_fails,
            "$descr_mode same key order no more than $allowed_fails times")
            or diag(
                "Key order was the same $random_same/$repeat times in",
                "RANDOM mode. This test is probabilistic so if the number",
                "is low and you re-run the tests and it does not fail",
                "again then you can ignore this test fail.");
    }

    # There is a small chance we got the same result. So allow a certain
    # number of fails.
    ok( $seed_change_same <= $allowed_fails,
        "$descr_mode same key order with munged seed no more " .
        "than $allowed_fails times" )
        or diag(
            "Key order was the same $random_same/$repeat times with",
            "a munged seed. This test is probabilistic so if the number",
            "is low and you re-run the tests and it does not fail",
            "again then you can ignore this test fail.");
    unlink_tempfiles();
}
