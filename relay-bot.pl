#!/usr/bin/perl -w
# $Id: relay-bot.pl,v 1.23 2002/07/04 19:29:56 freiheit Exp $

use strict;
use lib qw:/usr/local/lib/site_perl ./:;
use Net::IRC;
use vars qw/@relay_channels %hosts @authorizations $nick/;

require 'relay-bot.config';

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
my $host;
foreach $host (keys %hosts) {
    my @server;
    if ( ref( $hosts{$host} ) ) {
	@server = @{$hosts{$host}};
    } else {
        @server = ($hosts{$host});
    }
    print "Starting up $host (@server)\n";
    foreach my $server (@server) {
        my $connect =  $irc->newconn(
				     Nick   => $nick,
				     Ircname => "Relay-bot for @relay_channels on $host ($server)",
				     Server => $server,
				      # Sometimes needed with multiple
                                      # ethernet cards. Actually problem in
                                      # Net::IRC :
                                     # LocalAddr => '66.92.186.143',
				    );
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
	    print "\t$who!".$event->userhost." lacks authorization\n";
	    $host->privmsg($_,"$who: $cmd: ".
			   @permdenied_msg[int rand($#permdenied_msg+1)]);
	}
	return;
    }

    print "$reverse_hosts{$host}!$who issued cmd '$cmd' args {@args}\n";
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
	print "$reverse_hosts{$self}!$_ joining channel\n";
	$self->join($_);
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
    
    my $desc = "*** $reverse_hosts{$self}!$channel".
    " names: @list";
    
    print "$reverse_hosts{$self}!$channel names: @list\n";
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
	sleep 1;
	print ".";
    }
    print "\n";
    my $network = $reverse_hosts{$self};
    my $server = $self->server;
    if ( ref( $hosts{$network} ) ) {
	$server = $hosts{$network}->[rand @{$hosts{$network}}];
    } else {
	$server = $hosts{$network};
    }
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
	print $reverse_hosts{$self}.'!'.($event->to())[0].
	": $args[0]\n";
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
    } else {
	print "The topic for $args[1] is \"$args[2]\".\n";
    }
}

print "Adding topic handler\n";
for (@irc) {
    $_->add_handler('topic',   \&on_topic);
}

sub public_msg {
    my $self = shift;
    my $event = shift;
    
    my $nick = $event->nick;
    my ($arg) = $event->args;
    my @args = $event->args;
    
    my @to = $event->to;
    
    return if $arg =~ m/^\<\w+(\@\w+)?\> /;
    return if $arg =~ m/^\* \w+(\@\w+)? /;
    
    my $n = $self->nick;
    if ($arg =~ /^(\Q$n\E[,:]\s*)?([\^\/!]\w+)(\s|$)/i) {
	$arg =~ s/^\Q$n\E[,:]\s*//i;
	print "$reverse_hosts{$self}!$to[0] cmd: <$nick> $arg\n";
	&cmd($arg,$nick,$self,$event);
	return;
    }
    
    print "$reverse_hosts{$self}!$to[0] <$nick> $arg\n";

    for my $server (@irc) {
	next if $server == $self;
        for my $to (@to) {
            $server->privmsg($to,"<$nick\@$reverse_hosts{$self}> $arg");
        }
    }
}

sub public_action {
    my ($self, $event) = @_;
    my ($nick, @args) = ($event->nick, $event->args);
    
#    print "ARGS: ", join(':', @args), "\n";
#    shift @args;

    my @to = $event->to;
    print $reverse_hosts{$self}.'!'.($event->to())[0]." $nick @args\n";
    
    for my $server (@irc) {
	next if $server == $self;
        for my $to (@to) {
            $server->privmsg($to,"* $nick\@$reverse_hosts{$self} @args");
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
	print "$reverse_hosts{$self}!$to[0] cmd: <$nick> $arg\n";
	&cmd($arg,$nick,$self,$event);
	return;
    }
    
    if($arg =~ m/^[<>]?(\w{1,16})\@(\w{1,16})[<>]?\s+(.*)/) {
	my $to = $1;
	my $net = $2;
	$arg = $3;
	print $reverse_hosts{$self}.'!'.($event->to())[0].
	"!$nick\@$reverse_hosts{$self} -> $to\@$net: $arg\n";
	if (exists $forward_hosts{$net}) {
	    my $server = $forward_hosts{$net};
	    $server->privmsg($to,">$nick\@$reverse_hosts{$self}< $arg");
	}
    } elsif($arg =~ m/^[<>]?(\w{1,16})[<>]?\s+(.*)/) {
	my $to = $1;
	$arg = $2;
	print $reverse_hosts{$self}.'!'.($event->to())[0].
	"!$nick\@$reverse_hosts{$self} -> $to: $arg\n";
	for my $server (@irc) {
	    next if $server == $self;
	    $server->privmsg($to,">$nick\@$reverse_hosts{$self}< $arg");
	}
    } else {
	print $reverse_hosts{$self}.'!'.($event->to())[0].
	"!$nick: $arg\n";
    }
}

sub on_join {
    my $self = shift;
    my $event = shift;
    
    my $nick = $event->nick;
    return if &samenick($nick);
    
    my ($channel) = ($event->to)[0];
    
    my @arg = $event->args;
    
    print( "*** join ".
           $reverse_hosts{$self}."!".
           ($event->to)[0].
           ": ".$event->nick." ".$event->userhost."\n");
    
    

    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** join ".
			     $reverse_hosts{$self}."!".
			     ($event->to)[0].
			     ": ".$event->nick." ".$event->userhost);
	}
    }
    
    # primitive.
    if ($event->userhost =~
	/\@adsl-63-197-80-100\.dsl\.snfc21\.pacbell\.net$/) {
	$self->mode($channel,'+o',$event->nick);
    }
    
}

sub on_nick_change {
    my $self = shift;
    my $event = shift;
    
    print $reverse_hosts{$self}."!".($event->to)[0]." nick change ".
    $event->nick." ".$event->userhost.join(' ',$event->args)."\n";
    
    return if &samenick($event->nick);
    
    for my $server (@irc) {
	next if $server==$self;
	for my $to ($event->to) {
	    $server->privmsg($to,"*** nick change ".
			     $reverse_hosts{$self}."!".($event->to)[0].
			     "!".$event->userhost.' to '.
			     join(' ',$event->args));
	}
    }
}

sub on_part {
    my $self = shift;
    my $event = shift;
    
    print $reverse_hosts{$self}."!".($event->to)[0]." chan part ".
    $event->nick." ".$event->userhost."\n";
    
    return if &samenick($event->nick);
    
    print( "*** part ".
	   $reverse_hosts{$self}."!".
	   ($event->to)[0].
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
    
    print $reverse_hosts{$self}."!".($event->to)[0]." kick ".
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

sub on_mode {
    my $self = shift;
    my $event = shift;
    
    print $reverse_hosts{$self}."!".($event->to)[0]." mode ".
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
    
    print $reverse_hosts{$self}."!".($event->to)[0]." umode ".
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
    
    print $reverse_hosts{$self}."!>".($event->to)[0]."< quit ".
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

print "Adding other handlers\n";
for (@irc) {
    $_->add_handler('public',  \&public_msg);
    $_->add_handler('msg',     \&private_msg);
    $_->add_handler('caction', \&public_action);
    $_->add_handler('join',    \&on_join);
    $_->add_handler('part',    \&on_part);
    $_->add_handler('nick',    \&on_nick_change);
    $_->add_handler('kick',    \&on_kick);
    $_->add_handler('mode',    \&on_mode);
    $_->add_handler('umode',   \&on_umode);
    $_->add_handler('quit',    \&on_quit);
}

sub samenick {
    my $n = shift;
    
    for (keys %forward_hosts) {
	return 1 if $forward_hosts{$_}->nick eq $n;
    }
    return 0;
}

print "starting with ",Net::IRC->VERSION,"\n";
$irc->start;
