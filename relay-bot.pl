#!/usr/bin/perl -w
# $Id: relay-bot.pl,v 1.37 2002/10/18 00:29:45 wepprop Exp $
my $version_number = "x.x";

use strict;
use lib qw:/usr/local/lib/site_perl ./:;
use Net::IRC;
use vars qw/@relay_channels %relay_channels_extra %hosts @authorizations $nick %config/;
use vars qw/@auto_ops/;

my $config_file_name = "relay-bot.config";

# Command line handler

my $unused_option = -1;

my %override = (
	       echo_public_msg =>    $unused_option,
	       echo_private_msg =>   $unused_option,
	       echo_public_action => $unused_option,
	       echo_join =>          $unused_option,
	       echo_part =>          $unused_option,
	       echo_nick =>          $unused_option,
	       echo_kick =>          $unused_option,
	       echo_cmode =>         $unused_option,
	       echo_umode =>         $unused_option,
	       echo_quit =>          $unused_option,
	       echo_topic =>         $unused_option,
	       daemonize =>          $unused_option,
	       logfile =>            "$unused_option",
	       interface_address =>  "",

);

my $valid_args = "acedhijklmnpqtuv";

for ( my $i = 0, my $interval = 1 ; $i <= $#ARGV ; $i += $interval ) {

	$interval = 1;	
	my $arg = $ARGV[$i];	

	SWITCH: {
		if( $arg =~ /^[-+][$valid_args]*([^$valid_args])/ ) {
			print "Invalid argument $1 contained in $arg\n";
			print "\t-h for help.\n"; 
			exit 1;
		
		}
		if( $arg =~ /^[-+][$valid_args]+([fhilv])/ ) {
			print "$1 may not be grouped with other args: $arg\n";
			exit 1;
		}

		# -a
		if( $arg =~ /^\-[$valid_args]*a/ ) {
			$override{echo_public_action} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*a/ ) {
			$override{echo_public_action} = 1;
		}
		
		# -c
		if( $arg =~ /^\-[$valid_args]*c/ ) {
			$override{echo_cmode} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*c/ ) {
			$override{echo_cmode} = 1;
		}
		
		# -d
		if( $arg =~ /^\-[$valid_args]*d/ ) {
			$override{daemonize} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*d/ ) {
			$override{daemonize} = 1;
		}
		
		# -e
		if( $arg =~ /^\-[$valid_args]*e/ ) {
			$override{echo_public_msg} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*e/ ) {
			$override{echo_public_msg} = 1;
		}

		# -f
		if( $arg =~ /^-f/ ) {
			$config_file_name = $ARGV[$i+1];
			$interval = 2;
			next SWITCH;	
		}

		# -h
		if( $arg =~ /^-h/ ) {
			print "   [+|-]a      Echo public actions   ";
			print "   -l <fname>  Specify log file      \n";

			print "   [+|-]c      Echo channel modes    ";
			print "   [+|-]m      Echo private msgs     \n";	

			print "   [+|-]d      Run in background     ";
			print "   [+|-]n      Echo nick changes     \n";	

			print "   [+|-]e      Echo channel msgs     ";
			print "   [+|-]p      Echo parts            \n";
	
			print "   -f <fname>  Specify config file   ";
			print "   [+|-]q      Echo quits            \n";	

			print "   -h          Command option help   ";
			print "   [+|-]t      Echo topic            \n";

			print "   -i <ipaddr> Specify interface     ";
			print "   [+|-]u      Echo user modes       \n";	
	
			print "   [+|-]j      Echo joins            ";
			print "   -v          Version information   \n";

			print "   [+|-]k      Echo kicks            \n";

                        print "\n+ enables option, - disables option\n";
			exit 0;
		}
		
		# -i
		if( $arg =~ /^-i/ ) {
			my $addr = $ARGV[$i+1];
			if( $addr =~ /\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/ ) {	
				$override{interface_address} = $addr;
				$interval = 2;
				next SWITCH;	
			} else {
				print "Invalid IP Address: $addr\n";
				exit 1;
			}
		}

		# -j
		if( $arg =~ /^\-[$valid_args]*j/ ) {
			$override{echo_join} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*j/ ) {
			$override{echo_join} = 1;
		}
		
		# -k
		if( $arg =~ /^\-[$valid_args]*k/ ) {
			$override{echo_kick} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*k/ ) {
			$override{echo_kick} = 1;
		}
		
		# -l 
		if( $arg =~ /^-l/ ) {
			$override{logfile} = $ARGV[$i+1];
			$interval = 2;
			next SWITCH;	
		}

		# -m
		if( $arg =~ /^\-[$valid_args]*m/ ) {
			$override{echo_private_msg} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*m/ ) {
			$override{echo_private_msg} = 1;
		}

		# -n
		if( $arg =~ /^\-[$valid_args]*n/ ) {
			$override{echo_nick} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*n/ ) {
			$override{echo_nick} = 1;
		}
		
		# -p
		if( $arg =~ /^\-[$valid_args]*p/ ) {
			$override{echo_part} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*p/ ) {
			$override{echo_part} = 1;
		}
		
		# -q
		if( $arg =~ /^\-[$valid_args]*q/ ) {
			$override{echo_quit} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*q/ ) {
			$override{echo_quit} = 1;
		}
		
		# -t
		if( $arg =~ /^\-[$valid_args]*t/ ) {
			$override{echo_topic} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*t/ ) {
			$override{echo_topic} = 1;
		}

		# -u  
		if( $arg =~ /^\-[$valid_args]*u/ ) {
			$override{echo_umode} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*u/ ) {
			$override{echo_umode} = 1;
		}
		
		# -v
		if( $arg =~ /^-v/ ) {
			print "relay-bot version $version_number\n";
			exit 0;
		}
	}
}

# Config file processing
require $config_file_name;

# In case the options are not present in the config file...
if ( !defined( %config ) ) {
    %config = (
	       echo_public_msg => 1,
	       echo_private_msg => 1,
	       echo_public_action => 1,
	       echo_join => 1,
	       echo_part => 1,
	       echo_nick => 1,
	       echo_kick => 1,
	       echo_cmode => 1,
	       echo_umode => 1,
	       echo_quit => 1,
	       echo_topic => 1,
	       daemonize => 0,
	       interface_address => "",
    );
}

# Override config file settings with command line args where req'd.
if ( $override{echo_public_msg} != $unused_option ) {
    $config{echo_public_msg} = $override{echo_public_msg};
}
if ( $override{echo_private_msg} != $unused_option ) {
    $config{echo_private_msg} = $override{echo_private_msg};
}
if ( $override{echo_public_action} != $unused_option ) {
    $config{echo_public_action} = $override{echo_public_action};
}
if ( $override{echo_join} != $unused_option ) {
    $config{echo_join} = $override{echo_join};
}
if ( $override{echo_part} != $unused_option ) {
    $config{echo_part} = $override{echo_part};
}
if ( $override{echo_nick} != $unused_option ) {
    $config{echo_nick} = $override{echo_nick};
}
if ( $override{echo_kick} != $unused_option ) {
    $config{echo_kick} = $override{echo_kick};
}
if ( $override{echo_cmode} != $unused_option ) {
    $config{echo_cmode} = $override{echo_cmode};
}
if ( $override{echo_umode} != $unused_option ) {
    $config{echo_umode} = $override{echo_umode};
}
if ( $override{echo_quit} != $unused_option ) {
    $config{echo_quit} = $override{echo_quit};
}
if ( $override{echo_topic} != $unused_option ) {
    $config{echo_topic} = $override{echo_topic};
}
if ( $override{interface_address} ne "" ) {
    $config{interface_address} = $override{interface_address};
}
if ( $override{daemonize} != $unused_option ) {
    $config{daemonize} = $override{daemonize};
}
if ( $override{logfile} ne "$unused_option" ) {
    $config{logfile} = $override{logfile};
}

# Actual IRC object...
my $irc = Net::IRC->new();
print "Created Net::IRC object\n";

if ( $config{daemonize} ) {
    require Proc::Daemon;
    Proc::Daemon::Init();
}

# Set up logging 

my $log_fspec;

if( $config{logfile} eq "" ) {
    $log_fspec = "> -";
}else {
    $log_fspec = "> ".$config{logfile};
}

open(LOGFILE, $log_fspec) or die "Can't open logfile: <$!>";  

# Actual IRC connections kept here, kinda...
my @irc;

my %cmd_pending = (
		   names => [], ping => [], kick => [], whatever => []
);

my %forward_hosts = ();
my %reverse_hosts = ();

print LOGFILE "Setting up hosts\n";
my $connect;
my $host;
foreach $host (keys %hosts) {
    my @server;
    if ( ref( $hosts{$host} ) ) {
	@server = @{$hosts{$host}};
    } else {
        @server = ($hosts{$host});
    }
    print LOGFILE "Starting up $host (@server)\n";
    foreach my $server (@server) {

	if( $config{interface_address} eq "" ) {
	    $connect =  $irc->newconn(
					 Nick   => $nick,
					 Ircname => "Relay-bot for @relay_channels on $host ($server)",
					 Server => $server,
	    );
	} else {
	    $connect =  $irc->newconn(
					 Nick   => $nick,
					 Ircname => "Relay-bot for @relay_channels on $host ($server)",
					 Server => $server,
					 LocalAddr => $config{interface_address},
	    );
	}

        if (defined($connect) && $connect) {
            push @irc, $connect;
	    $forward_hosts{$host} = $connect;
            $reverse_hosts{"$connect"} = $host;
            print LOGFILE "$host ($server) successful\n";
	    last;
        } else {
	    print LOGFILE "$host ($server) failed\n";
        }
    }
}
print LOGFILE "Done with hosts\n";

sub cmd_auth {
    my ($nuh,$cmd) = @_;
    ref $nuh eq 'Net::IRC::Event' and $nuh = $nuh->nick.'!'.$nuh->userhost;
    $cmd or return undef;
    
    for my $o (@authorizations) {
	# some people think Perl's a write-only language... :)
	($o->[0] =~ tr/A-Z/A-Z/ ? $nuh =~ /$o->[0]/ : $nuh =~ /$o->[0]/i) and
	return ($o->[1]->{'*'} || $o->[1]->{$cmd});
    }
    0;
}

my @allcmd = ('names','restart','die','quit','leave','part','join');
my %cmd_map = ( 'die' => 'quit', 'leave' => 'part', map { ($_=>$_) } @allcmd );
my @permdenied_msg = ( 'Permission denied.', 'Insufficient privelege',
			'u r ! l33t w1t m3' );
sub cmd {
    my ($cmd,$who,$host,$event) = @_;
    my $cmd0 = $cmd;
    my @args;
    my $mode = 'public';
    my $n = $host->nick;
    $cmd =~ /^\^(\w+)/ and $mode = 'silent';
    $cmd =~ s/^[\^\/](\w+)/$1/;
    ($cmd,@args) = split /\s+/, $cmd;
    
    unless ($cmd = $cmd_map{lc $cmd}) {
	# $host->privmsg(($event->to)[0],"$who: unknown command $cmd0");
	return;
    }
    unless (&cmd_auth("$who!".$event->userhost,$cmd)) {
	for ($event->to) {
	    print LOGFILE "\t$who!".$event->userhost." lacks authorization\n";
	    $host->privmsg($_,"$who: $cmd: ".
			   @permdenied_msg[int rand($#permdenied_msg+1)]);
	}
	return;
    }

    print LOGFILE "$who\@$reverse_hosts{$host} issued cmd '$cmd' args {@args}\n";
    if (lc $cmd eq 'names') {
	my $channel = shift @args || ($event->to)[0];
	for my $server (@irc) {
	    next if $server == $host;
	    push @{$cmd_pending{names}},
	    [ $mode eq 'public' ? ($event->to)[0] : $who,
	      $server, $host, $event ];
	    print LOGFILE "/// names command issued on $channel\n";
	    $server->names($channel);
	}
    } elsif ($cmd eq 'join') {
	my $channel = shift @args || return;
	my $network = shift @args || '';
	$network eq 'here' and $network = $reverse_hosts{$host};
	
	unless (grep($_ eq $channel,@relay_channels)) {
	    $host->privmsg(($event->to)[0],
			   "$who: I'm not allowed in $channel.");
	    return;
	}
	if ($network and $forward_hosts{$network}) {
	    $forward_hosts{$network}->join($channel);
	} else {
	    for my $server (@irc) {
		push @{$cmd_pending{'join'}},
		[ $mode eq 'public' ? ($event->to)[0] : $who,
		  $server, $host, $event, $channel ];
		$server->join($channel);
	    }
	}
    } elsif ($cmd eq 'part') {
	my $channel = shift @args || return;
	grep($_ eq $channel,@relay_channels) or return; # report error here
	my $network = shift @args || '';
	if ($network and $forward_hosts{$network}) {
	    $forward_hosts{$network}->part($channel);
	} else {
	    for my $server (@irc) {
		push @{$cmd_pending{part}},
		[ $mode eq 'public' ? ($event->to)[0] : $who,
		  $server, $host, $event, $channel ];
		$server->part($channel);
	    }
	}
    } elsif ($cmd eq 'quit' or $cmd eq 'restart') {
	for my $server (@irc) {
	    $server->quit($cmd eq 'quit' ? "Bailing" : "Restarting".
			  " ($who)");
	}
	$cmd eq 'quit' and exit;
	sleep 5;
	exec($0,@ARGV) || exec('perl',$0,@ARGV) || die "exec($0,@ARGV): $!";
    }
}


sub on_connect {
    my $self = shift;
    
    for (@relay_channels) {
	print LOGFILE "$_\@$reverse_hosts{$self} joining channel\n";
	$self->join($_);
    }
}

print LOGFILE "Adding connect handler\n";
for (@irc) {
    $_->add_global_handler('376', \&on_connect);
}

# Handles some messages you get when you connect
sub on_init {
    my ($self, $event) = @_;
    my (@args) = ($event->args);
    shift (@args);
    
    print LOGFILE "*** @args\n";
}

print LOGFILE "Adding init handler\n";
for (@irc) {
    $_->add_global_handler([ 251,252,253,254,302,255 ], \&on_init);
}

# Prints the names of people in a channel when we enter.
sub on_names {
    my ($self, $event) = @_;
    my (@list, $channel) = ($event->args);    # eat yer heart out, mjd!
    
    # splice() only works on real arrays. Sigh.
    ($channel, @list) = splice @list, 2;
    
    my $desc = "*** $channel\@$reverse_hosts{$self}".
    " names: @list";
    
    print LOGFILE "$channel\@$reverse_hosts{$self} names: @list\n";
    return unless @{$cmd_pending{names}};
    my $n=0;
    for my $w (@{$cmd_pending{names}}) {
	if ($w->[1] == $self and
	    ($w->[3]->to)[0] eq $channel) {
	    print LOGFILE "/// command channel=$channel who=$w->[0]\n";
	    $w->[2]->privmsg($w->[0],$desc);
	    splice(@{$cmd_pending{names}},$n,1);
	    last;
	}
	$n++;
    }
    for my $server (@irc) {
	next if $server == $self;
	$server->privmsg(($event->to)[0],$desc);
#	for my $to ($event->to) {
#	    print LOGFILE "/// privmsg to=$to desc=$desc\n";
#       }
    }
}

print LOGFILE "Adding names handler\n";
for (@irc) {
    $_->add_global_handler(353, \&on_names);
}

# Yells about incoming CTCP PINGs.
sub on_ping {
    my ($self, $event) = @_;
    my $nick = $event->nick;
    
    print LOGFILE "$reverse_hosts{$self} ping from $nick\n";
    
    $self->ctcp_reply($nick, join (' ', ('PING', $event->args)));
}

print LOGFILE "Adding ping handler\n";
for (@irc) {
    $_->add_handler('cping',  \&on_ping);
}

# Gives lag results for outgoing PINGs.
sub on_ping_reply {
    my ($self, $event) = @_;
    my ($args) = ($event->args)[1];
    my ($nick) = $event->nick;
    
    $args = time - $args;
    print LOGFILE "*** CTCP PING reply from $nick: $args sec.\n";
}

print LOGFILE "Adding ping reply handler\n";
for (@irc) {
    $_->add_handler('crping', \&on_ping_reply);
}

# Change our nick if someone stole it.
sub on_nick_taken {
    my ($self) = shift;
    
    $self->nick(substr($self->nick, -1) . substr($self->nick, 0, 8));
}

print LOGFILE "Adding nick taken handler\n";
for (@irc) {
    $_->add_global_handler(433, \&on_nick_taken);
}

# Reconnect to the server when we die.
sub on_disconnect {
    my ($self, $event) = @_;
    
    print LOGFILE "Disconnected from ", $event->from(), " (",
    ($event->args())[0], "). Attempting to reconnect...\n";
    print LOGFILE "Sleeping";
    local $| = 1;
    foreach (1..3) {
	sleep 1;
	print LOGFILE ".";
    }
    print LOGFILE "\n";
    my $network = $reverse_hosts{$self};
    my $server = $self->server;
    if ( ref( $hosts{$network} ) ) {
	$server = $hosts{$network}->[rand @{$hosts{$network}}];
    } else {
	$server = $hosts{$network};
    }
    print LOGFILE "Connecting to $server\n";
    $self->connect(Server => $server) || on_disconnect(@_);
}

print LOGFILE "Adding disconnect handler\n";
for (@irc) {
    $_->add_global_handler('disconnect', \&on_disconnect);
}

# Look at the topic for a channel you join.
sub on_topic {
    my ($self, $event) = @_;
    my @args = $event->args();
    my @to = $event->to();
    
    # Note the use of the same handler sub for different events.
    
    if (&samenick($event->nick)) {
	return 0;
    }
    
    if ($event->type() eq 'notopic') {
	print LOGFILE "No topic set for $args[1].\n";
	
        # If it's being done _to_ the channel, it's a topic change.
    } elsif ($event->type() eq 'topic' and $event->to()) {
	print LOGFILE ( ($event->to())[0] . '@'.$reverse_hosts{$self}.
	": $args[0]\n" );
	for my $server (@irc) {
	    next if $server==$self;
	    for my $to (@to) {
		$server->privmsg($to,"*** topic ".
				 $reverse_hosts{$self}.
				 "!".($event->to())[0].
				 "!".$event->nick.
				 ": $args[0]");
		$server->topic(($event->to())[0],$args[0]);
	    }
	}

        for my $to (@to) {
            next unless exists $relay_channels_extra{$to};
            my @channels_to = @{$relay_channels_extra{$to}};
            for my $channel_to (@channels_to) {
                for my $server (@irc) {
                    $server->privmsg($channel_to,"*** topic ".
				     $reverse_hosts{$self}.
				     "!".($event->to())[0].
				     "!".$event->nick.
				     ": $args[0]");
		    $server->topic($channel_to,$args[0]);
                }
            }
        }
    } else {
	print LOGFILE "The topic for $args[1] is \"$args[2]\".\n";
    }
}

if ($config{echo_topic}) {
    print LOGFILE "Adding topic handler\n";
    for (@irc) {
        $_->add_handler('topic',   \&on_topic);
    }
}

sub public_msg {
    my $self = shift;
    my $event = shift;
    
    my $nick = $event->nick;
    my ($arg) = $event->args;
    my @args = $event->args;
    
    my @to = $event->to;
    
    return if $arg =~ m/^\<\w+(\@\w+)?\> /; # looks like it's another relay-bot
    return if $arg =~ m/^\*+ \w+(\@\w+)? /;  # also looks like a bot
    
    # Look for commands:
    my $n = $self->nick;
    if ($arg =~ /^(\Q$n\E[,:]\s*)?([\^\/!]\w+)(\s|$)/i) {
	$arg =~ s/^\Q$n\E[,:]\s*//i;
	print LOGFILE "$to[0]\@$reverse_hosts{$self} cmd: <$nick> $arg\n";
	&cmd($arg,$nick,$self,$event);
	return;
    }
    
    print LOGFILE "$to[0]\@$reverse_hosts{$self} <$nick> $arg\n";

    for my $server (@irc) {
	next if $server == $self;
        for my $to (@to) {
            $server->privmsg($to,"<$nick\@$reverse_hosts{$self}> $arg");
        }
    }

    for my $to (@to) {
        next unless exists $relay_channels_extra{$to};
        my @channels_to = @{$relay_channels_extra{$to}};
        for my $channel_to (@channels_to) {
            for my $server (@irc) {
                $server->privmsg($channel_to,"<$nick\@$reverse_hosts{$self}> $arg");
            }
        }
    }
}

sub public_action {
    my ($self, $event) = @_;
    my ($nick, @args) = ($event->nick, $event->args);
    
#    print LOGFILE "ARGS: ", join(':', @args), "\n";
#    shift @args;

    my @to = $event->to;
    print LOGFILE ( ($event->to())[0].'@'.$reverse_hosts{$self}." $nick @args\n");
    
    for my $server (@irc) {
	next if $server == $self;
        for my $to (@to) {
            $server->privmsg($to,"* $nick\@$reverse_hosts{$self} @args");
        }
    }
    for my $to (@to) {
        next unless exists $relay_channels_extra{$to};
        my @channels_to = @{$relay_channels_extra{$to}};
        for my $channel_to (@channels_to) {
            for my $server (@irc) {
                $server->privmsg($channel_to, "* $nick\@$reverse_hosts{$self} @args");
            }
        }
    }
}

sub private_msg {
    my $self = shift;
    my $event = shift;
    my @to = $event->to();
    my $n = $self->nick;
    my $nick = $event->nick;
    my @arg = $event->args;
    my $arg = "@arg";
    
    if ($arg =~ /^(\Q$n\E[,:]\s*)?([\^\/]\w+)(\s|$)/i) {
	$arg =~ s/^\Q$n\E[,:]\s*//i;
	print LOGFILE "$to[0]\@$reverse_hosts{$self} cmd: <$nick> $arg\n";
	&cmd($arg,$nick,$self,$event);
	return;
    }
    
    if($arg =~ m/^[<>]?(\w{1,16})\@(\w{1,16})[<>]?\s+(.*)/) {
	my $to = $1;
	my $net = $2;
	$arg = $3;
	print LOGFILE ( ($event->to())[0].'@'.$reverse_hosts{$self}.
	"!$nick\@$reverse_hosts{$self} -> $to\@$net: $arg\n");
	if (exists $forward_hosts{$net}) {
	    my $server = $forward_hosts{$net};
	    $server->privmsg($to,">$nick\@$reverse_hosts{$self}< $arg");
	}
    } elsif($arg =~ m/^[<>]?(\w{1,16})[<>]?\s+(.*)/) {
	my $to = $1;
	$arg = $2;
	print LOGFILE ( ($event->to())[0].'@'.$reverse_hosts{$self}.
	"!$nick\@$reverse_hosts{$self} -> $to: $arg\n");
	for my $server (@irc) {
	    next if $server == $self;
	    $server->privmsg($to,">$nick\@$reverse_hosts{$self}< $arg");
	}
    } else {
	print LOGFILE ( ($event->to())[0].'@'.$reverse_hosts{$self}.
	"!$nick: $arg\n");
    }
}

sub on_join {
    my $self = shift;
    my $event = shift;

    my ($channel) = ($event->to)[0];
    
    # Auto-ops - still primitive, requires regexp knowledge

    if( defined( @auto_ops ) ) {
	foreach my $auto_op_member (@auto_ops) {
	    if ( $event->{userhost} =~ /^$auto_op_member$/ ) {
		$self->mode($channel,'+o',$event->nick);
		last;
	    }
	}
    }

    return unless $config{echo_join};

    my $nick = $event->nick;
    return if &samenick($nick);
    
    my @arg = $event->args;
    
    print LOGFILE ( "*** join ".
           ($event->to)[0].'@'.
           $reverse_hosts{$self}.
           ": ".$event->nick." ".$event->userhost."\n");
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** join ".
	                     ($event->to)[0].'@'.
			     $reverse_hosts{$self}.
			     ": ".$event->nick." ".$event->userhost);
	}
    }
    for my $to ($event->to) {
        next unless exists $relay_channels_extra{$to};
        my @channels_to = @{$relay_channels_extra{$to}};
        for my $channel_to (@channels_to) {
            for my $server (@irc) {
	        $server->privmsg($channel_to,"*** join ".
	                         ($event->to)[0].'@'.
                                  $reverse_hosts{$self}.
			         ": ".$event->nick." ".$event->userhost);
            }
        }
    }
}

sub on_nick_change {
    my $self = shift;
    my $event = shift;
    
    print LOGFILE ( ($event->to)[0].'@'.$reverse_hosts{$self}." nick change ".
    $event->nick." ".$event->userhost.join(' ',$event->args)."\n");
    
    return if &samenick($event->nick);
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** nick change ".
			     ($event->to)[0].'@'.$reverse_hosts{$self}.
			     "!".$event->userhost.' to '.
			     join(' ',$event->args));
	}
    }
}

sub on_part {
    my $self = shift;
    my $event = shift;
    
    print LOGFILE ( ($event->to)[0].'@'.$reverse_hosts{$self}." chan part ".
    $event->nick." ".$event->userhost."\n");
    
    return if &samenick($event->nick);
    
    print LOGFILE ( "*** part ".
	   ($event->to)[0].'@'.
	   $reverse_hosts{$self}.
	   ": ".$event->nick." ".$event->userhost);
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** part ".
			     $reverse_hosts{$self}."!".($event->to)[0].
			     ": ".$event->nick." ".$event->userhost);
	}
    }
}

sub on_kick {
    my $self = shift;
    my $event = shift;
    
    print LOGFILE $reverse_hosts{$self}."!".($event->to)[0]." kick ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    return if &samenick($event->nick);
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** ".
			     $reverse_hosts{$self}."!".($event->to)[0].
			     ": ".$event->nick." kicked ".join(' ',$event->args));
	}
    }
}

sub on_cmode {
    my $self = shift;
    my $event = shift;
    
    print LOGFILE $reverse_hosts{$self}."!".($event->to)[0]." mode ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** channel mode change ".
			     $reverse_hosts{$self}."!".$event->nick.' '.
			     join(' ',$event->args));
	}
    }
}

sub on_umode {
    my $self = shift;
    my $event = shift;
    
    print LOGFILE $reverse_hosts{$self}."!".($event->to)[0]." umode ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** user mode change ".
			     $reverse_hosts{$self}."!".$event->nick.' '.
			     join(' ',$event->args));
	}
    }
}

sub on_quit {
    my $self = shift;
    my $event = shift;
    
    print LOGFILE $reverse_hosts{$self}."!>".($event->to)[0]."< quit ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to (@relay_channels) {
	    $server->privmsg($to,"*** signoff ".
			     $reverse_hosts{$self}."!".$event->nick.' '.
			     '('.join(' ',$event->args).')');
	}
    }
}

print LOGFILE "Adding other handlers\n";
for (@irc) {
    if ($config{echo_public_msg}) {
        $_->add_handler('public',  \&public_msg);
    }
    if ($config{echo_public_action}) {
        $_->add_handler('caction', \&public_action);
    }
    if($config{echo_private_msg}) {
        $_->add_handler('msg',     \&private_msg);
    }

    $_->add_handler('join',    \&on_join);

    if($config{echo_part}) {
        $_->add_handler('part',    \&on_part);
    }
    if($config{echo_nick}) {
        $_->add_handler('nick',    \&on_nick_change);
    }
    if($config{echo_kick}) {
        $_->add_handler('kick',    \&on_kick);
    }
    if($config{echo_cmode}) {
        $_->add_handler('mode',    \&on_cmode);
    }
    if($config{echo_umode}) {
        $_->add_handler('umode',   \&on_umode);
    }
    if($config{echo_quit}) {
        $_->add_handler('quit',    \&on_quit);
    }
}

sub samenick {
    my $n = shift;
    
    for (keys %forward_hosts) {
	return 1 if $forward_hosts{$_}->nick eq $n;
    }
    return 0;
}

print LOGFILE "starting with ",Net::IRC->VERSION,"\n";
$irc->start;
