package Process::Govern;

use 5.010;
use strict;
use warnings;

# VERSION

use Exporter qw(import);
our @EXPORT_OK = qw(govern_process);

sub govern_process {
    my %args = @_;

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

    my $pid;
    if ($args{single}) {
        defined($args{pid_dir}) or die "Please specify pid_dir\n";
        require Proc::PID::File;
        $pid = Proc::PID::File->new(dir=>$args{pid_dir}, name=>$name,
                                    verify=>1);
        die "Already running" if $pid->alive;
    }

    ###

    my $out = sub {
        print $_[0];
    };

    my $err;
    my $fwr;
    if ($args{log_stderr}) {
        require File::Write::Rotate;
        my %fa = %{$args{log_stderr}};
        $fa{dir}    //= "/var/log";
        $fa{prefix}   = $name;
        $fwr = File::Write::Rotate->new(%fa);
        $err = sub {
            print STDERR $_[0];
            # XXX prefix with timestamp, how long script starts,
            $_[0] =~ s/^/STDERR: /mg;
            $fwr->write($_[0]);
        };
    } else {
        $err = sub {
            print STDERR $_[0];
        };
    }

    if ($pid && $pid->alive) {
        die "Already running";
    }

    my $start_time = time();
    require IPC::Run;
    my $h = IPC::Run::start($cmd, \*STDIN, $out, $err);

    my $res;
    my $time;
    while (1) {
        unless ($h->pumpable) {
            $h->finish;
            $res = $h->result;
            last;
        }
        unless ($h->pump_nb) {
            sleep 1; # XXX sleep in finer granularity
        }
        if (defined $args{timeout}) {
            my $time = time();
            if ($time - $start_time >= $args{timeout}) {
                $err->("Timeout ($args{timeout}s), killing process ...\n");
                $h->kill_kill;
                # mark with a special exit code that it's a timeout
                $res = 201;
                last;
            }
        }
    }
    exit $res;
}

1;
# ABSTRACT: Run child process and govern its various aspects

=for Pod::Coverage ^(new)$

=head1 SYNOPSIS

Use command-line utility:

 % govproc \
       --timeout 3600 \
       --log-stderr-dir        /var/log/myapp/ \
       --log-stderr-size       16M \
       --log-stderr-histories  12 \
   /path/to/myapp

Use directly as Perl module:

 use Process::Govern qw(govern_process);
 govern_process(
     name       => 'myapp',
     command    => '/path/to/myapp',
     timeout    => 3600,
     stderr_log => {
         dir       => '/var/log/myapp',
         size      => '16M',
         histories => 12,
     },
 );


=head1 DESCRIPTION

Process::Govern is a process manager. It is designed in the spirit of
L<Perinci::Sub::Wrapper>, that is, a single wrapper (parent process) that
manages various aspects of the "wrapee" (child process).

It comes with a command-line interface, L<govproc>.

Background story: I first created this module to record STDERR output of scripts
that I run from cron. The scripts already log debugging information using
L<Log::Any> to an autorotated log file (using L<Log::Dispatch::FileRotate>, via
L<Log::Any::Adapter::Log4perl>, via L<Log::Any::App>). However, when the scripts
warn/die, or when the programs that the scripts execute emit messages to STDERR,
they do not get recorded. Thus, every script is then run through B<govproc>.
From there, B<govproc> naturally gets additional features.

Currently the following governing functionalities are available:

=over

=item * execution time limit

=item * logging of STDERR output

=item * prevent multiple instances from running simultaneously

=back

In the future the following features are also planned or contemplated:

=over

=item * CPU time limit

=item * memory limit

With an option to autorestart if process' memory size grow out of limit.

=item * other resource usage limit

=item * fork/start multiple process

=item * autorestart on die/failure

=item * set (CPU) nice level

=item * set I/O nice level (scheduling priority/class)

=item * loadwatch (pause when system load is too high)

=item * limit STDIN input, STDOUT output?

=item * trap/handle some signals for the child process?

=item * set UID/GID?

=item * provide daemon functionality?

=item * provide network server functionality?

Inspiration: djb's B<tcpserver>.

=item * set/clean environment variables

=back


=head1 FUNCTIONS

=head2 govern_process(%args)

Run child process and govern its various aspects. It basically uses L<IPC::Run>
and a loop to check various conditions during the lifetime of the child process.
Known arguments (required argument is marked with C<*>):

=over

=item * command* => STR | ARRAYREF | CODE

Program to run. Passed to IPC::Run's C<start()>.

=item * name => STRING

Should match regex C</\A\w+\z/>. Used in several ways, e.g. passed as C<prefix>
in L<File::Write::Rotate>'s constructor as well as used as name of PID file.

If not given, will be taken from command.

=item * timeout => INT

Apply execution time limit, in seconds. After this time is reached, process (and
all its descendants) are first sent the TERM signal. If after 30 seconds pass
some processes still survive, they are sent the KILL signal.

The killing is implemented using L<IPC::Run>'s C<kill_kill()>.

Upon timeout, exit code is set to 201.

=item * log_stderr => HASH

Specify logging for STDERR. Logging will be done using L<File::Write::Rotate>.
Known hash keys: C<dir> (STR, defaults to /var/log, directory, preferably
absolute, where the log file(s) will reside, should already exist and be
writable, will be passed to File::Write::Rotate's constructor), C<size> (INT,
also passed to File::Write::Rotate's constructor), C<histories> (INT, also
passed to File::Write::Rotate's constructor), C<period> (STR, also passed to
File::Write::Rotate's constructor).

=item * single => BOOL

If set to true, will prevent running multiple instances simultaneously.
Implemented using L<Proc::PID::File>. You will also have to set C<pid_dir>.

=item * pid_dir => STR

Directory to put PID file in. Relevant if C<single> is set to true. Default to
C</var/log>.

=back


=head1 FAQ


=head1 CAVEATS

Not tested on Win32.


=head1 SEE ALSO

Process::Govern attempts (or will attempt, some day) to provide the
functionality (or some of the functionality) of the builtins/modules/programs
listed below:

=over

=item * Starting/autorestarting

djb's B<supervise>, http://cr.yp.to/daemontools/supervise.html

=item * Pausing under high system load

B<loadwatch>

cPanel also include a program called B<cpuwatch>.

=item * Preventing multiple instances of program running simultaneously

L<Proc::PID::File>, L<Sys::RunAlone>

=item * Execution time limit

alarm() (but alarm() cannot be used to timeout external programs started by
system()/backtick).

L<Sys::RunUntil>

=item * Logging

djb's B<multilog>, http://cr.yp.to/daemontools/multilog.html

=back

=cut
