#!/usr/bin/perl -w
# $Id: relay-bot.pl,v 1.47 2003/05/12 00:29:19 freiheit Exp $
my $version_number = "x.x";

use strict;
use lib qw:/usr/local/lib/site_perl ./:;
use Net::IRC;
use vars qw/@relay_channels %relay_channels_extra %hosts @authorizations $nick %config/;
use vars qw/%Relays %ReceiveMap @auto_ops/;

my $config_file_name = "relay-bot.config";

# Command line handler

my $unused_option = -1;

my %override = (
		nick =>              "$unused_option",
		disconnect_sleep =>   $unused_option,
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
		echo_set_topic =>     $unused_option,
		echo_topic =>         $unused_option,
		daemonize =>          $unused_option,
		logfile =>           "$unused_option",
		logfile_buffering =>  $unused_option,
		interface_address =>  "",
);

my $valid_args = "abcdefhijklmnpqstuvw";

for ( my $i = 0, my $interval = 1 ; $i <= $#ARGV ; $i += $interval ) {

	$interval = 1;	
	my $arg = $ARGV[$i];	

	SWITCH: {
		if( $arg =~ /^[-+][$valid_args]*([^$valid_args])/ ) {
			print "Invalid argument $1 contained in $arg\n";
			print "\t-h for help.\n"; 
			exit 1;
		}

		if( $arg =~ /^[-+][$valid_args]+([fhilvw])/ ) {
			print "$1 may not be grouped with other args: $arg\n";
			exit 1;
		}

		# -a enable/disable echo of public actions
		if( $arg =~ /^\-[$valid_args]*a/ ) {
			$override{echo_public_action} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*a/ ) {
			$override{echo_public_action} = 1;
		}

		# -b enable/disable logfile buffering
		if( $arg =~ /^\-[$valid_args]*b/ ) {
			$override{logfile_buffering} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*b/ ) {
			$override{logfile_buffering} = 1;
		}
		
		# -c enable/disable echo of channel mode changes
		if( $arg =~ /^\-[$valid_args]*c/ ) {
			$override{echo_cmode} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*c/ ) {
			$override{echo_cmode} = 1;
		}
		
		# -d enable/disable running detached (background)
		if( $arg =~ /^\-[$valid_args]*d/ ) {
			$override{daemonize} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*d/ ) {
			$override{daemonize} = 1;
		}
		
		# -e enable/disable echo of public messages
		if( $arg =~ /^\-[$valid_args]*e/ ) {
			$override{echo_public_msg} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*e/ ) {
			$override{echo_public_msg} = 1;
		}

		# -f set config filename
		if( $arg =~ /^-f/ ) {
			$config_file_name = $ARGV[$i+1];
			$interval = 2;
			next SWITCH;	
		}

		# -h program help
		if( $arg =~ /^-h/ ) {
			print "   [+|-]a      Echo public actions   ";
			print "   -l <fname>  Specify log file      \n";

			print "   [+|-]b      Buffer log output     ";
			print "   [+|-]m      Echo private msgs     \n";

			print "   [+|-]c      Echo channel modes    ";
			print "   [+|-]n      Echo nick changes     \n";	

			print "   [+|-]d      Run in background     ";
			print "   [+|-]p      Echo parts            \n";
	
			print "   [+|-]e      Echo channel msgs     ";
			print "   [+|-]q      Echo quits            \n";	

			print "   -f <fname>  Specify config file   ";
			print "   [+|-]s      Set topic by echo     \n";

			print "   -h          Command option help   ";
			print "   [+|-]t      Echo topic change     \n";

			print "   -i <ipaddr> Specify interface     ";	
			print "   [+|-]u      Echo user modes       \n";

			print "   [+|-]j      Echo joins            ";
			print "   -v          Version information   \n";

			print "   [+|-]k      Echo kicks            ";
			print "   -w nick     Specify nick/handle   \n";


                        print "\n+ enables option, - disables option\n";
			exit 0;
		}
		
		# -i set interface (by ip address) to use on multi-homed box
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

		# -j enable/disable echo of joins
		if( $arg =~ /^\-[$valid_args]*j/ ) {
			$override{echo_join} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*j/ ) {
			$override{echo_join} = 1;
		}
		
		# -k enable/disable echo of kicks
		if( $arg =~ /^\-[$valid_args]*k/ ) {
			$override{echo_kick} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*k/ ) {
			$override{echo_kick} = 1;
		}
		
		# -l set log file
		if( $arg =~ /^-l/ ) {
			$override{logfile} = $ARGV[$i+1];
			$interval = 2;
			next SWITCH;	
		}

		# -m enable/disable echo of private msgs
		if( $arg =~ /^\-[$valid_args]*m/ ) {
			$override{echo_private_msg} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*m/ ) {
			$override{echo_private_msg} = 1;
		}

		# -n enable/disable echo of nick changes
		if( $arg =~ /^\-[$valid_args]*n/ ) {
			$override{echo_nick} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*n/ ) {
			$override{echo_nick} = 1;
		}
		
		# -p enable/disable echo of parts
		if( $arg =~ /^\-[$valid_args]*p/ ) {
			$override{echo_part} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*p/ ) {
			$override{echo_part} = 1;
		}
		
		# -q echo quits
		if( $arg =~ /^\-[$valid_args]*q/ ) {
			$override{echo_quit} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*q/ ) {
			$override{echo_quit} = 1;
		}

		# -s set topic in echo channels
		if( $arg =~ /^\-[$valid_args]*s/ ) {
			$override{echo_set_topic} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*s/ ) {
			$override{echo_set_topic} = 1;
		}

		
		# -t enable/disable echo of topic change
		if( $arg =~ /^\-[$valid_args]*t/ ) {
			$override{echo_topic} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*t/ ) {
			$override{echo_topic} = 1;
		}

		# -u enable/disable echo of user mode changes
		if( $arg =~ /^\-[$valid_args]*u/ ) {
			$override{echo_umode} = 0;
		}
		if( $arg =~ /^\+[$valid_args]*u/ ) {
			$override{echo_umode} = 1;
		}
		
		# -v print version number
		if( $arg =~ /^-v/ ) {
			print "relay-bot version $version_number\n";
			exit 0;
		}

		# -w set nick (who) of relay bot
		if( $arg =~ /^-w/ ) {
			$override{nick} = $ARGV[$i+1];
			$interval = 2;
			next SWITCH;	
		}
	}
}

# Config file processing
require $config_file_name;

# In case the options are not present in the config file...
%config = (
	   nick => "relay-bot",
           disconnect_sleep => 3,
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
	   echo_set_topic => 1,
	   echo_topic => 1,
	   daemonize => 0,
	   interface_address => "",
	   logfile_buffering => 1,
           # Anything in here will override the above
           %config
);

# This for reverse compatibility with old config files

if( defined( $nick ) ) { $config{nick} = $nick; }

# Override config file settings with command line args where req'd.

if ( $override{nick} ne "$unused_option" ) {
    $config{nick} = $override{nick};
}
if ( $override{disconnect_sleep} != $unused_option ) {
    $config{disconnect_sleep} = $override{disconnect_sleep};
}
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
if ( $override{echo_set_topic} != $unused_option ) {
    $config{echo_set_topic} = $override{echo_set_topic};
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
if ( $override{logfile_buffering} != $unused_option ) {
    $config{logfile_buffering} = $override{logfile_buffering};
}

# This block of code exists to provide reverse compatibility with
# the previous %hosts and @relay_channels structures.

if( defined( %hosts ) ) {

    # Old style config file - create the %Relays structure

    %Relays = ();

    foreach my $network (keys %hosts) {

	my $group = 0;

	foreach my $entry (@relay_channels) {

	    my $channel_name;
	    my $channel_password;

	    if( $entry =~ /^([^ ]+)\s([^ ]+)$/ ) {
		$channel_name = $1;
		$channel_password = $2;
	    } else {
		$channel_name = $entry;
		$channel_password = "";
	    }

            if( ref( $hosts{$network} ) ) {
	        $Relays{$network}{servers} = $hosts{$network};
            } else {
		# This for reverse compatibility with obsolete config files
                $Relays{$network}{servers} = [ $hosts{$network} ];
	    }

	    $Relays{$network}{channels}{$channel_name}{passwd}=$channel_password;
	    $Relays{$network}{channels}{$channel_name}{group} = $group;
	    $Relays{$network}{channels}{$channel_name}{rcv} = 1;
	    $Relays{$network}{channels}{$channel_name}{xmit} = 1;

	    ++$group;
	}
    }
} else {

    # @relay_channels is retained (for now) for convenience in a few lines

    @relay_channels = ();

    foreach my $network (keys %Relays) {

	foreach my $channel (keys %{ $Relays{$network}{channels} } ) {

	    push @relay_channels, $channel;

	    last;
	}
	
	last;
    }
}

# This block creates the ReceiveMap, which is used when deciding which
# channels to relay to.

foreach my $network (keys %Relays) {

    foreach my $channel (keys %{ $Relays{$network}{channels} } ) {

        my $group = $Relays{$network}{channels}{$channel}{group};
        my $rcv   = $Relays{$network}{channels}{$channel}{rcv  };
		
        $ReceiveMap{$group}{$network}{$channel} = $rcv;
    }
}

# If requested, run in the background.
# Note:  The Proc::Daemon module sets umask 0
# and chdir's to \, which breaks logging unless
# the umask and PWD are restored.

if( $config{daemonize} ) {
    my $current_umask = umask;
    my $current_dir = $ENV{PWD};

    require Proc::Daemon;
    Proc::Daemon::Init();
    
    umask $current_umask;
    chdir $current_dir;
}

# Set up logging

if( $config{logfile} ne "" ) {
    open( LOGFILE, "> ".$config{logfile} ) or die "Can't open logfile: <$!>";
    select( LOGFILE );
    $| = 1 unless $config{logfile_buffering};
    open( STDERR, ">&LOGFILE" );
}

# Interrupt handler

$SIG{INT} = \&signal_interrupt;

# Actual IRC object...
my $irc = Net::IRC->new();
print "Created Net::IRC object\n";

# Actual IRC connections kept here, kinda...
my @irc;

my %cmd_pending = (
		   names => [], ping => [], kick => [], whatever => []
);

my %forward_hosts = ();
my %reverse_hosts = ();

print "Setting up hosts\n";
my $connect;
my $host;
foreach $host (keys %Relays) {
    my @server;

    @server = @{ $Relays{$host}{servers} };

    print "Starting up $host (@server)\n";

    foreach my $server (@server) {

	if( $config{interface_address} eq "" ) {
	    $connect =  $irc->newconn(
					 Nick   => $config{nick},
					 Ircname => "Relay-bot for @relay_channels on $host ($server)",
					 Server => $server,
	    );
	} else {
	    $connect =  $irc->newconn(
					 Nick   => $config{nick},
					 Ircname => "Relay-bot for @relay_channels on $host ($server)",
					 Server => $server,
					 LocalAddr => $config{interface_address},
	    );
	}

        if (defined($connect) && $connect) {
            push @irc, $connect;
	    $forward_hosts{$host} = $connect;
            $reverse_hosts{"$connect"} = $host;
            print "$host ($server) successful\n";
	    last;
        } else {
	    print "$host ($server) failed\n";
        }
    }
}
print "Done with hosts\n";

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

my @allcmd = ('names','restart','die','quit','leave','part','join','add');
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
	    print "\t$who!".$event->userhost." lacks authorization\n";
	    $host->privmsg($_,"$who: $cmd: ".
			   @permdenied_msg[int rand($#permdenied_msg+1)]);
	}
	return;
    }

    print "$who\@$reverse_hosts{$host} issued cmd '$cmd' args {@args}\n";
    if (lc $cmd eq 'names') {
	my $channel = shift @args || ($event->to)[0];
	for my $server (@irc) {
	    next if $server == $host;
	    push @{$cmd_pending{names}},
	    [ $mode eq 'public' ? ($event->to)[0] : $who,
	      $server, $host, $event ];
	    print "/// names command issued on $channel\n";
	    $server->names($channel);
	}
    } 
    elsif ($cmd eq 'join') {
	my $channel_or_group = shift @args || return;
	my $network = shift @args || '';
	my @networks = keys %Relays;
	my @groups   = keys %ReceiveMap;

	$network eq 'here' and $network = $reverse_hosts{$host};

	if( $network and $forward_hosts{$network} ) {
	    my $channel = $channel_or_group;
	    my @channels = keys %{ $Relays{$network}{channels} };

	    if( grep( $_ eq $channel, @channels ) ) {
		$forward_hosts{$network}->join($channel);
	    } 
	    else {
		$host->privmsg(($event->to)[0],
			       "$who: I'm not allowed in $channel"."@"."$network.");
	    }
	} 
	else {
	    my $group = $channel_or_group;

	    if( grep( $_ eq $group, @groups ) ) {
		foreach my $network (keys %{ $ReceiveMap{$group} } ) {
		    foreach my $channel (keys %{ $ReceiveMap{$group}{$network} } ) {
			my $server = $forward_hosts{$network};
			$server->join($channel);

#			push @{$cmd_pending{'join'}},
#			    [ $mode eq 'public' ? ($event->to)[0] : $who,
#			      $server, $host, $event, $channel ];
		    }
		}
	    } 
	    else {
		$host->privmsg(($event->to)[0],
			       "Invalid argument: $group is not a valid group\n");
	    }
	}
    } elsif ($cmd eq 'part') {
	my $channel_or_group = shift @args || return;
	my $network = shift @args || '';
	my @networks = keys %Relays;
	my @groups   = keys %ReceiveMap;

	$network eq 'here' and $network = $reverse_hosts{$host};

	if( $network and $forward_hosts{$network} ) {
	    my $channel = $channel_or_group;
	    my @channels = keys %{ $Relays{$network}{channels} };

	    if( grep( $_ eq $channel, @channels ) ) {
		$forward_hosts{$network}->part($channel);
	    } 
	    else {
		$host->privmsg(($event->to)[0],
			       "$who: I'm not in $channel"."@"."$network.");
	    }
	} 
	else {
	    my $group = $channel_or_group;

	    if( grep( $_ eq $group, @groups ) ) {
		foreach my $network (keys %{ $ReceiveMap{$group} } ) {
		    foreach my $channel (keys %{ $ReceiveMap{$group}{$network} } ) {
			my $server = $forward_hosts{$network};
			$server->part($channel);
		    }
		}
	    } 
	    else {
		$host->privmsg(($event->to)[0],
			       "Invalid argument: $group is not a valid group\n");
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
    } elsif( $cmd eq 'add' ) {
	my $channel  = shift @args || return;
	my $network  = shift @args || return;
	my $group    = shift @args || return;
	my $rcv      = shift @args || 1;
	my $xmit     = shift @args || 1;
	my $password = shift @args || '';

	unless( $rcv =~ /^[01]$/ ) {
	    $host->privmsg( ($event->to)[0],
			    "Invalid ADD argument: rcv must be 1 or 0\n");
	    return;
	}
	unless( $xmit =~ /^[01]$/ ) {
	    $host->privmsg( ($event->to)[0],
			    "Invalid ADD argument: xmit must be 1 or 0\n");
	    return;
	}
	unless( $forward_hosts{$network} ) {
	    $host->privmsg( ($event->to)[0],
			    "Invalid ADD argument host: $network not in list\n");
	    return;
	}

	print "ADDing $channel"."@"."$network in $group with password $password; rcv $rcv, xmit $xmit\n";

	$Relays{$network}{channels}{$channel}{passwd} = $password;
	$Relays{$network}{channels}{$channel}{group}  = $group;
	$Relays{$network}{channels}{$channel}{rcv}    = $rcv;
	$Relays{$network}{channels}{$channel}{xmit}   = $xmit;

        $ReceiveMap{$group}{$network}{$channel} = $rcv;
    }
}


sub on_connect {
    my $self = shift;
    my $network = $reverse_hosts{$self};
    
    foreach my $channel (keys %{ $Relays{$network}{channels} } ) {
	print "Joining channel $channel on network $network\n";
	$self->join($channel." " .$Relays{$network}{channels}{$channel}{passwd} );
    }
}

print "Adding connect handler\n";
for (@irc) {
    $_->add_global_handler('376', \&on_connect);
}

# Handles some messages you get when you connect
sub on_init {
    my ($self, $event) = @_;
    my (@args) = ($event->args);
    shift (@args);
    
    print "*** @args\n";
}

print "Adding init handler\n";
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
    
    print "$channel\@$reverse_hosts{$self} names: @list\n";
    return unless @{$cmd_pending{names}};
    my $n=0;
    for my $w (@{$cmd_pending{names}}) {
	if ($w->[1] == $self and
	    ($w->[3]->to)[0] eq $channel) {
	    print "/// command channel=$channel who=$w->[0]\n";
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
#	    print "/// privmsg to=$to desc=$desc\n";
#       }
    }
}

print "Adding names handler\n";
for (@irc) {
    $_->add_global_handler(353, \&on_names);
}

# Yells about incoming CTCP PINGs.
sub on_ping {
    my ($self, $event) = @_;
    my $nick = $event->nick;
    
    print "$reverse_hosts{$self} ping from $nick\n";
    
    $self->ctcp_reply($nick, join (' ', ('PING', $event->args)));
}

print "Adding ping handler\n";
for (@irc) {
    $_->add_handler('cping',  \&on_ping);
}

# Gives lag results for outgoing PINGs.
sub on_ping_reply {
    my ($self, $event) = @_;
    my ($args) = ($event->args)[1];
    my ($nick) = $event->nick;
    
    $args = time - $args;
    print "*** CTCP PING reply from $nick: $args sec.\n";
}

print "Adding ping reply handler\n";
for (@irc) {
    $_->add_handler('crping', \&on_ping_reply);
}

# Change our nick if someone stole it.
sub on_nick_taken {
    my ($self) = shift;
    
    $self->nick(substr($self->nick, -1) . substr($self->nick, 0, 8));
}

print "Adding nick taken handler\n";
for (@irc) {
    $_->add_global_handler(433, \&on_nick_taken);
}

# Reconnect to the server when we die.
sub on_disconnect {
    my ($self, $event) = @_;
    
    print "Disconnected from ", $event->from(), " (",
    ($event->args())[0], "). Attempting to reconnect...\n";
    print "Sleeping";
    local $| = 1;
    foreach (1..3) {
	sleep $config{disconnect_sleep};
	print ".";
    }
    print "\n";
    my $network = $reverse_hosts{$self};
    my @serverlist = @{ $Relays{$network}{servers} };
    my $last_server_index = $#serverlist;
    my $random_server_index = int( rand( $last_server_index ) );
    
    my $server = $serverlist[ $random_server_index ];
    
    print "Connecting to $server\n";
    $self->connect(Server => $server) || on_disconnect(@_);
}

print "Adding disconnect handler\n";
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
	print "No topic set for $args[1].\n";
	
        # If it's being done _to_ the channel, it's a topic change.
    } elsif ($event->type() eq 'topic' and $event->to()) {

	print "" . ($event->to())[0] . '@'.$reverse_hosts{$self}.
	": $args[0]\n";

        my $original_network = $reverse_hosts{$self};

        foreach my $original_channel (@to) {

            my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

            if( $channel_send_enable ) {

                my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

                foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                    foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                        next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                        my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                        if( $channel_receive_enable ) {

                            my $server = $forward_hosts{$echo_network};

                            if( $config{echo_topic} ) {
				$server->privmsg($echo_channel,"*** topic ".
						 $original_network.
						 "!".$original_channel.
						 "!".$event->nick.
						 ": $args[0]");
			    }

                            if( $config{echo_set_topic} ) {
				$server->topic($echo_channel,$args[0]);
			    }
                        }
                    }
                }
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
	print "The topic for $args[1] is \"$args[2]\".\n";
    }
}

if( $config{echo_topic} || $config{echo_set_topic} ) {
    print "Adding topic handler\n";
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
	print "$to[0]\@$reverse_hosts{$self} cmd: <$nick> $arg\n";
	&cmd($arg,$nick,$self,$event);
	return;
    }
    
    print "$to[0]\@$reverse_hosts{$self} <$nick> $arg\n";

    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"<$nick\@$reverse_hosts{$self}> $arg");
		     }
		}
	    }
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
    
#    print "ARGS: ", join(':', @args), "\n";
#    shift @args;

    my @to = $event->to;
    print ( ($event->to())[0].'@'.$reverse_hosts{$self}." $nick @args\n");
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"* $nick\@$reverse_hosts{$self} @args");
		     }
		}
	    }
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
	print "$to[0]\@$reverse_hosts{$self} cmd: <$nick> $arg\n";
	&cmd($arg,$nick,$self,$event);
	return;
    }
    
    if($arg =~ m/^[<>]?(\w{1,16})\@(\w{1,16})[<>]?\s+(.*)/) {
	my $to = $1;
	my $net = $2;
	$arg = $3;
	print "". ($event->to())[0].'@'.$reverse_hosts{$self}.
	"!$nick\@$reverse_hosts{$self} -> $to\@$net: $arg\n";
	if (exists $forward_hosts{$net}) {
	    my $server = $forward_hosts{$net};
	    $server->privmsg($to,">$nick\@$reverse_hosts{$self}< $arg");
	}
    } elsif($arg =~ m/^[<>]?(\w{1,16})[<>]?\s+(.*)/) {
	my $to = $1;
	$arg = $2;
	print "". ($event->to())[0].'@'.$reverse_hosts{$self}.
	"!$nick\@$reverse_hosts{$self} -> $to: $arg\n";
	for my $server (@irc) {
	    next if $server == $self;
	    $server->privmsg($to,">$nick\@$reverse_hosts{$self}< $arg");
	}
    } else {
	print "". ($event->to())[0].'@'.$reverse_hosts{$self}.
	"!$nick: $arg\n";
    }
}

sub on_join {
    my $self = shift;
    my $event = shift;
    my @to = $event->to();
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
    
    print "*** join ".
           ($event->to)[0].'@'.
           $reverse_hosts{$self}.
           ": ".$event->nick." ".$event->userhost."\n";
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"*** join ".
					  ($event->to)[0].'@'.
					  $reverse_hosts{$self}.
					  ": ".$event->nick." ".$event->userhost);
		     }
		}
	    }
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
    my @to = $event->to();
    
    print "".( ($event->to)[0].'@'.$reverse_hosts{$self}." nick change ".
    $event->nick." ".$event->userhost.join(' ',$event->args)."\n");
    
    return if &samenick($event->nick);
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"*** nick change ".
					  ($event->to)[0].'@'.$reverse_hosts{$self}.
					  "!".$event->userhost.' to '.
					  join(' ',$event->args));
		    }
		}
	    }
	}
    }
}

sub on_part {
    my $self = shift;
    my $event = shift;
    my @to = $event->to();
    
    print "".( ($event->to)[0].'@'.$reverse_hosts{$self}." chan part ".
    $event->nick." ".$event->userhost."\n");
    
    return if &samenick($event->nick);
    
    print "".( "*** part ".
	   ($event->to)[0].'@'.
	   $reverse_hosts{$self}.
	   ": ".$event->nick." ".$event->userhost);
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"*** part ".
					  $reverse_hosts{$self}."!".($event->to)[0].
					  ": ".$event->nick." ".$event->userhost);
		     }
		}
	    }
	}
    }
}

sub on_kick {
    my $self = shift;
    my $event = shift;
    my @to = $event->to();
    
    print $reverse_hosts{$self}."!".($event->to)[0]." kick ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    return if &samenick($event->nick);
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"*** ".
					  $reverse_hosts{$self}."!".($event->to)[0].
					  ": ".$event->nick." kicked ".join(' ',$event->args));
		     }
		}
	    }
	}
    }
}

sub on_cmode {
    my $self = shift;
    my $event = shift;
    my @to = $event->to();
    
    print $reverse_hosts{$self}."!".($event->to)[0]." mode ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"*** channel mode change ".
					  $reverse_hosts{$self}."!".$event->nick.' '.
					  join(' ',$event->args));
		     }
		}
	    }
	}
    }
}

sub on_umode {
    my $self = shift;
    my $event = shift;
    my @to = $event->to();
    
    print $reverse_hosts{$self}."!".($event->to)[0]." umode ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"*** user mode change ".
					  $reverse_hosts{$self}."!".$event->nick.' '.
					  join(' ',$event->args));
		     }
		}
	    }
	}
    }
}

sub on_quit {
    my $self = shift;
    my $event = shift;
    my @to = $event->to();
    
    print $reverse_hosts{$self}."!>".($event->to)[0]."< quit ".
    $event->nick." ".join(' ',$event->args)."\n";
    
    my $original_network = $reverse_hosts{$self};

    foreach my $original_channel (@to) {

        my $channel_send_enable = $Relays{$original_network}{channels}{$original_channel}{xmit};

        if( $channel_send_enable ) {

            my $relay_group = $Relays{$original_network}{channels}{$original_channel}{group};

            foreach my $echo_network (keys %{ $ReceiveMap{$relay_group} } ) {

                foreach my $echo_channel (keys %{ $ReceiveMap{$relay_group}{$echo_network} } ) {

                    next if( ($echo_network eq $original_network) && ($echo_channel eq $original_channel) );

                    my $channel_receive_enable = $ReceiveMap{$relay_group}{$echo_network}{$echo_channel};

                    if( $channel_receive_enable ) {

                         my $server = $forward_hosts{$echo_network};

			 $server->privmsg($echo_channel,"*** signoff ".
					  $reverse_hosts{$self}."!".$event->nick.' '.
					  '('.join(' ',$event->args).')');
		     }
		}
	    }
	}
    }
}

print "Adding other handlers\n";
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

sub signal_interrupt {
    my $signal = shift;
    print "Exiting due to SIG$signal\n";
    my $logfile = select;
    close( $logfile );
    exit 0;
    }

print "starting with ",Net::IRC->VERSION,"\n";
$irc->start;
