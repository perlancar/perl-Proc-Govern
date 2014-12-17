package Proc::Govern;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use Exporter qw(import);
our @EXPORT_OK = qw(govern_process);

our %SPEC;

use Time::HiRes qw(sleep);

sub new {
    my ($class) = @_;
    bless {}, $class;
}

sub _suspend {
    my $self = shift;
    my $h = $self->{h};
    say "D:Suspending program ..." if $self->{debug};
    if (@{ $h->{KIDS} }) {
        my @args = (STOP => (map { $_->{PID} } @{ $h->{KIDS} }));
        if ($self->{args}{killfam}) {
            #say "D:killfam ".join(" ", @args) if $self->{debug};
            Proc::Killfam::killfam(@args);
        } else {
            #say "D:kill ".join(" ", @args) if $self->{debug};
            kill @args;
        }
    }
    $self->{suspended} = 1;
}

sub _resume {
    my $self = shift;
    my $h = $self->{h};
    say "D:Resuming program ..." if $self->{debug};
    if (@{ $h->{KIDS} }) {
        my @args = (CONT => (map { $_->{PID} } @{ $h->{KIDS} }));
        if ($self->{args}{killfam}) {
            #say "D:killfam ".join(" ", @args) if $self->{debug};
            Proc::Killfam::killfam(@args);
        } else {
            #say "D:kill ".join(" ", @args) if $self->{debug};
            kill @args;
        }
    }
    $self->{suspended} = 0;
}

sub _kill {
    my $self = shift;
    my $h = $self->{h};
    $self->_resume if $self->{suspended};
    say "D:Killing program ..." if $self->{debug};
    $self->{restart} = 0;
    $h->kill_kill;
}

$SPEC{govern_process} = {
    v => 1.1,
    summary => 'Run child process and govern its various aspects',
    args => {
        name => {
            schema => 'str*',
        },
        command => {
            schema => ['any*' => of => ['str*', ['array*' => of => 'str*']]],
            req => 1,
        },
        single_instance => {
            schema => [bool => default => 0],
        },
        on_multiple_instance => {
            schema => ['str*' => in => ['exit']],
        },
        load_watch => {
            schema => [bool => default => 0],
        },
        load_check_every => {
            schema => [int => default => 10],
        },
        load_high_limit => {
            schema => ['any*' => of => [[int => default => 1.25], 'code*']],
        },
        load_low_limit => {
            schema => ['any*' => of => [[int => default => 0.25], 'code*']],
        },
        killfam => {
            summary => 'Instead of kill, use killfam (kill family of process)',
            schema  => 'bool',
            description => <<'_',

This can be useful e.g. to control load more successfully, if the
load-generating processes are the subchildren of the one we're governing.

This requires `Proc::Killfam` CPAN module, which is installed separately.

_
        },
        log_stdout => {
            summary => 'Will be passed as arguments to File::Write::Rotate',
            schema => ['hash*' => keys => {
                dir       => 'str*',
                size      => 'str*',
                histories => 'int*',
            }],
        },
        show_stdout => {
            schema => [bool => default => 1],
        },
        log_stderr => {
            summary => 'Will be passed as arguments to File::Write::Rotate',
            schema => ['hash*' => keys => {
                dir       => 'str*',
                size      => 'str*',
                histories => 'int*',
            }],
        },
        show_stderr => {
            schema => [bool => default => 1],
        },
        timeout => {
            schema => ['int*'],
        },
        restart => {
            schema => [bool => default => 1],
        },
        # not yet defined
        #restart_delay => {
        #    schema => [int => default => 0],
        #},
        #check_alive => {
        #    # not yet defined, can supply a custom coderef, or specify some
        #    # standard checks like TCP/UDP connection to some port, etc.
        #    schema => 'any*',
        #},
    },
    result_naked => 1,
    result => {
        summary => "Child's exit code",
        schema => 'int',
    },
};
sub govern_process {
    my $self;
    if (ref $_[0]) {
        $self = shift;
    } else {
        $self = __PACKAGE__->new;
    }

    my %args = @_;
    $self->{args} = \%args;

    require Proc::Killfam if $args{killfam};

    my $debug = $ENV{DEBUG};
    $self->{debug} = $debug;

    my $exitcode;

    my $cmd = $args{command};
    defined($cmd) or die "Please specify command\n";

    my $name = $args{name};
    if (!defined($name)) {
        $name = ref($cmd) eq 'ARRAY' ? $cmd->[0] : ref($cmd) ? 'prog' : $cmd;
        $name =~ s!.*/!!; $name =~ s/\W+/_/g;
        length($name) or $name = "prog";
    }
    defined($name) or die "Please specify name\n";
    $name =~ /\A\w+\z/ or die "Invalid name, please use letters/numbers only\n";
    $self->{name} = $name;

    if ($args{single_instance}) {
        my $pid_dir = $args{pid_dir} // "/var/run";
        require Proc::PID::File;
        if (Proc::PID::File->running(dir=>$pid_dir, name=>$name, verify=>1)) {
            if ($args{on_multiple_instance} &&
                    $args{on_multiple_instance} eq 'exit') {
                $exitcode = 202; goto EXIT;
            } else {
                warn "Program $name already running\n";
                $exitcode = 202; goto EXIT;
            }
        }
    }

    my $showout = $args{show_stdout} // 1;
    my $showerr = $args{show_stderr} // 1;

    my $lw     = $args{load_watch} // 0;
    my $lwfreq = $args{load_check_every} // 10;
    my $lwhigh = $args{load_high_limit}  // 1.25;
    my $lwlow  = $args{load_low_limit}   // 0.25;

    ###

    my $out;
    if ($args{log_stdout}) {
        require File::Write::Rotate;
        my %fwrargs = %{$args{log_stdout}};
        $fwrargs{dir}    //= "/var/log";
        $fwrargs{prefix}   = $name;
        my $fwr = File::Write::Rotate->new(%fwrargs);
        $out = sub {
            print STDOUT $_[0] if $showout;
            # XXX prefix with timestamp, how long script starts,
            $_[0] =~ s/^/STDOUT: /mg;
            $fwr->write($_[0]);
        };
    } else {
        $out = sub {
            print STDOUT $_[0] if $showout;
        };
    }

    my $err;
    if ($args{log_stderr}) {
        require File::Write::Rotate;
        my %fwrargs = %{$args{log_stderr}};
        $fwrargs{dir}    //= "/var/log";
        $fwrargs{prefix}   = $name;
        my $fwr = File::Write::Rotate->new(%fwrargs);
        $err = sub {
            print STDERR $_[0] if $showerr;
            # XXX prefix with timestamp, how long script starts,
            $_[0] =~ s/^/STDERR: /mg;
            $fwr->write($_[0]);
        };
    } else {
        $err = sub {
            print STDERR $_[0] if $showerr;
        };
    }

    my $start_time; # for timeout
    my ($to, $h);

    my $do_start = sub {
        $start_time = time();
        require IPC::Run;
        say "D:(Re)starting program $name ..." if $debug;
        $to = IPC::Run::timeout(1);
        #$self->{to} = $to;
        $h  = IPC::Run::start($cmd, \*STDIN, $out, $err, $to)
            or die "Can't start program: $?\n";
        $self->{h} = $h;
    };

    $do_start->();

    local $SIG{INT} = sub {
        say "D:Received INT signal" if $debug;
        $self->_kill;
        exit 1;
    };

    local $SIG{TERM} = sub {
        say "D:Received TERM signal" if $debug;
        $self->_kill;
        exit 1;
    };

    my $chld_handler;
    $self->{restart} = $args{restart};
    $chld_handler = sub {
        $SIG{CHLD} = $chld_handler;
        if ($self->{restart}) {
            say "D:Child died" if $debug;
            $do_start->();
        }
    };
    local $SIG{CHLD} = $chld_handler if $args{restart};

    my $lastlw_time;

  MAIN_LOOP:
    while (1) {
        #say "D:main loop" if $debug;
        if (!$self->{suspended}) {
            # re-set timer, it might be reset by suspend/resume?
            $to->start(1);

            unless ($h->pumpable) {
                $h->finish;
                $exitcode = $h->result;
                last MAIN_LOOP;
            }

            eval { $h->pump };
            my $everr = $@;
            die $everr if $everr && $everr !~ /^IPC::Run: timeout/;
        } else {
            sleep 1;
        }
        my $now = time();

        if (defined $args{timeout}) {
            if ($now - $start_time >= $args{timeout}) {
                $err->("Timeout ($args{timeout}s), killing child ...\n");
                $self->_kill;
                # mark with a special exit code that it's a timeout
                $exitcode = 124;
                last MAIN_LOOP;
            }
        }

        if ($lw && (!$lastlw_time || $lastlw_time <= ($now-$lwfreq))) {
            say "D:Checking load" if $debug;
            if (!$self->{suspended}) {
                my $is_high;
                if (ref($lwhigh) eq 'CODE') {
                    $is_high = $lwhigh->($h);
                } else {
                    require Unix::Uptime;
                    my @load = Unix::Uptime->load();
                    $is_high = $load[0] >= $lwhigh;
                }
                if ($is_high) {
                    say "D:Load is too high" if $debug;
                    $self->_suspend;
                }
            } else {
                my $is_low;
                if (ref($lwlow) eq 'CODE') {
                    $is_low = $lwlow->($h);
                } else {
                    require Unix::Uptime;
                    my @load = Unix::Uptime->load();
                    $is_low = $load[0] <= $lwlow;
                }
                if ($is_low) {
                    say "D:Load is low" if $debug;
                    $self->_resume;
                }
            }
            $lastlw_time = $now;
        }

    } # MAINLOOP

  EXIT:
    return $exitcode || 0;
}

1;
# ABSTRACT:

=for Pod::Coverage ^(new)$

=head1 SYNOPSIS

To use via command-line (in most cases):

 % govproc \
       --timeout 3600 \
       --log-stderr-dir        /var/log/myapp/ \
       --log-stderr-size       16M \
       --log-stderr-histories  12 \
   /path/to/myapp

To use directly as Perl module:

 use Proc::Govern qw(govern_process);
 govern_process(
     name       => 'myapp',
     command    => '/path/to/myapp',
     timeout    => 3600,
     log_stderr => {
         dir       => '/var/log/myapp',
         size      => '16M',
         histories => 12,
     },
 );


=head1 DESCRIPTION

Proc::Govern is a child process manager. It is meant to be a convenient bundle
(a single parent/monitoring process) for functionalities commonly needed when
managing a child process. It comes with a command-line interface, L<govproc>.

Background story: I first created this module to record STDERR output of scripts
that I run from cron. The scripts already log debugging information using
L<Log::Any> to an autorotated log file (using L<Log::Dispatch::FileRotate>, via
L<Log::Any::Adapter::Log4perl>, via L<Log::Any::App>). However, when the scripts
warn/die, or when the programs that the scripts execute emit messages to STDERR,
they do not get recorded. Thus, every script is then run through B<govproc>.
From there, B<govproc> naturally gets additional features like timeout,
preventing running multiple instances, and so on.

Currently the following governing functionalities are available:

=over

=item * logging of STDOUT & STDERR output to an autorotated file

=item * execution time limit

=item * preventing multiple instances from running simultaneously

=item * load watch

=item * autorestart

=back

In the future the following features are also planned or contemplated:

=over

=item * CPU time limit

=item * memory limit

With an option to autorestart if process' memory size grow out of limit.

=item * other resource usage limit

=item * fork/start multiple processes

=item * set (CPU) nice level

=item * set I/O nice level (scheduling priority/class)

=item * limit STDIN input, STDOUT/STDERR output?

=item * trap/handle some signals for the child process?

=item * set UID/GID?

=item * provide daemon functionality?

=item * provide network server functionality?

Inspiration: djb's B<tcpserver>.

=item * set/clean environment variables

=back


=head1 EXIT CODES

Below is the list of exit codes that Proc::Govern uses:

=over

=item * 124

Timeout. The exit code is also used by B<timeout>.

=item * 202

Another instance is already running (when C<single_instance> option is true).

=back


=head1 FUNCTIONS

=head2 govern_process(%args) => INT

Run child process and govern its various aspects. It basically uses L<IPC::Run>
and a loop to check various conditions during the lifetime of the child process.
Known arguments (required argument is marked with C<*>):

=over

=item * command* => STR | ARRAYREF

Program to run. Passed to IPC::Run's C<start()>.

=item * name => STRING

Should match regex C</\A\w+\z/>. Used in several places, e.g. passed as
C<prefix> in L<File::Write::Rotate>'s constructor as well as used as name of PID
file.

If not given, will be taken from command.

=item * timeout => INT

Apply execution time limit, in seconds. After this time is reached, process (and
all its descendants) are first sent the TERM signal. If after 30 seconds pass
some processes still survive, they are sent the KILL signal.

The killing is implemented using L<IPC::Run>'s C<kill_kill()>.

Upon timeout, exit code is set to 124.

=item * show_stderr => BOOL (default: 1)

Can be used to turn off STDERR output. If you turn this off and set
C<log_stderr>, STDERR output will still be logged but not displayed to screen.

=item * log_stderr => HASH

Specify logging for STDERR. Logging will be done using L<File::Write::Rotate>.
Known hash keys: C<dir> (STR, defaults to /var/log, directory, preferably
absolute, where the log file(s) will reside, should already exist and be
writable, will be passed to File::Write::Rotate's constructor), C<size> (INT,
also passed to File::Write::Rotate's constructor), C<histories> (INT, also
passed to File::Write::Rotate's constructor), C<period> (STR, also passed to
File::Write::Rotate's constructor).

=item * show_stdout => BOOL (default: 1)

Just like C<show_stdout>, but for STDOUT.

=item * log_stdout => HASH

Just like C<log_stderr>, but for STDOUT.

=item * single_instance => BOOL

If set to true, will prevent running multiple instances simultaneously.
Implemented using L<Proc::PID::File>. You will also normally have to set
C<pid_dir>, unless your script runs as root, in which case you can use the
default C</var/run>.

=item * pid_dir => STR (default: /var/run)

Directory to put PID file in. Relevant if C<single> is set to true.

=item * on_multiple_instance => STR

Can be set to 'exit' to silently exit when there is already a running instance.
Otherwise, will print an error message 'Program <NAME> already running'.

=item * load_watch => BOOL (default: 0)

If set to 1, enable load watching. Program will be suspended when system load is
too high and resumed if system load returns to a lower limit.

=item * load_high_limit => INT|CODE (default: 1.25)

Limit above which program should be suspended, if load watching is enabled. If
integer, will be compared against C<< Unix::Uptime->load >>'s C<$load1> value.
Alternatively, you can provide a custom routine here, code should return true if
load is considered too high.

=item * load_low_limit => INT|CODE (default: 0.25)

Limit below which program should resume, if load watching is enabled. If
integer, will be compared against C<< Unix::Uptime->load >>'s C<$load1> value.
Alternatively, you can provide a custom routine here, code should return true if
load is considered low.

=item * load_check_every => INT (default: 10)

Frequency of load checking, in seconds.

=item * restart => BOOL (default: 0)

If set to true, do restart.

=back

Planned arguments: restart_delay, check_alive.

Return value: command exit code.


=head1 ENVIRONMENT

=head2 DEBUG => bool

If set to true, will display debugging output to STDERR, e.g. when
stopping/starting a process.


=head1 FAQ

=head2 Why use Proc::Govern?

The main feature this module offers is convenience: it creates a single parent
process to monitor child process. This fact is more pronounced when you need to
monitor lots of child processes. If you use, on the other hand, separate
parent/monitoring process for timeout and then a separate one for CPU watching,
and so on, there will potentially be a lot more processes running on the system.
Compare for example:

 % govproc --timeout 10 --load-watch CMD

which only creates one monitoring process, versus:

 % timeout 10s loadwatch CMD

which will create two parent processes (three actually, B<loadwatch> apparently
forks first).


=head1 CAVEATS

Not yet tested on Win32.


=head1 TODO

=over

=item * Govern multiple processes instead of just one

It's only natural that we expand to this, to reduce the number of monitor
process.

We want to be able to set options for all processes or on a per-process basis.
For example: when load watching, all processes can be stopped and resumed using
the same high/load criteria, but some processes might want to have a different
criteria. The same goes with timeout.

Some options are for a per-process, e.g. capturing stderr.

If we support multiple commands, e.g. C<< commands => ['cmd1', ['cmd2', 'arg']]
>> then we'll also need to return exit codes for each command, e.g. C<< [0, 124]
>>.

We should exit only after all child processes terminate. But when a child exits,
a hook can be defined e.g. C<on_child_exit>.

=item * Allow specifying time point (instead of duration) for timeout?

For example, we might want to say "this command should not run past midnight".

In general, we might also want to allow specifying a coderef for flexible
timeout criteria?

=item * Print messages when stopping/resuming due to load control

Like B<loadwatch> does:

 Fri Mar 14 16:17:52 2014: load too high, stopping.
 Fri Mar 14 16:18:52 2014: load low, continuing.

=item * Option to not use File::Write::Rotate for logging STDOUT/STDERR

If command is output-heavy, FWR will become a significant overhead. Unless FWR
has the option of skipping logging (I'm contemplating on this) ...

=back


=head1 SEE ALSO

Proc::Govern uses L<IPC::Run> at its core.

L<IPC::Cmd> also uses IPC::Run (as well as L<IPC::Open3> on systems that do not
have IPC::Run installed or on some archaic systems that do not support IPC::Run)
and its C<run_forked()> routine also has some of Proc::Govern's functionalities
like capturing stdout and stderr, timeout, hiding (discarding) output. If you
only need those functionalities, you can use IPC::Cmd as it is a core module.

Proc::Govern attempts (or will attempt, some day) to provide the functionality
(or some of the functionality) of the builtins/modules/programs listed below:

=over

=item * Starting/autorestarting

djb's B<supervise>, http://cr.yp.to/daemontools/supervise.html

=item * Pausing under high system load

B<loadwatch>. This program also has the ability to run N copies of program and
interactively control stopping/resuming via Unix socket.

cPanel also includes a program called B<cpuwatch>.

=item * Preventing multiple instances of program running simultaneously

L<Proc::PID::File>, L<Sys::RunAlone>

=item * Execution time limit

B<timeout>.

alarm() (but alarm() cannot be used to timeout external programs started by
system()/backtick).

L<Sys::RunUntil>

=item * Logging

djb's B<multilog>, http://cr.yp.to/daemontools/multilog.html

=back

Although not really related, L<Perinci::Sub::Wrapper>. This module also bundles
functionalities like timeout, retries, argument validation, etc into a single
function wrapper.

=cut
