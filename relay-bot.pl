#!/usr/bin/perl -w
# $Id: relay-bot.pl,v 1.10 2000/11/28 06:56:33 eric Exp $
use strict;

use lib qw:/usr/local/lib/site_perl/:;

use Net::IRC;

my $irc = Net::IRC->new();

my @irc;

my %hosts = (
    efnet => 'irc.west.gblx.net', # EFnet
#    'irc.east.gblx.net', # EFnet
#    'irc.lightning.net', # EFnet

    undernet => 'us.undernet.org',
#'atlanta.ga.US.Undernet.Org',
    #'lasvegas.nv.us.undernet.org',
    #'austin.tx.us.undernet.org',

    # 'us.dal.net',

    openprojects => 'irc.openprojects.net',

    #'irc.chelmsford.com', # Newnet
);

my %reverse_hosts = ();

my $host;
foreach $host (keys %hosts) {
    my $connect =  $irc->newconn(
	Nick   => 'Fandanta',
	Server => $hosts{$host});
    if (defined($connect) && $connect) {
        push @irc, $connect;
	$reverse_hosts{"$connect"} = $host;
    }
}

#my $efnet = $irc->newconn(Nick   => 'Fandanta',
#                          Server => 'irc.west.gblx.net',
#);
#
#my $under = $irc->newconn(Nick   => 'Fandanta',
##                          Server => 'lasvegas.nv.us.undernet.org',
##                          Server => 'austin.tx.us.undernet.org',
#                          Server => 'atlanta.ga.US.Undernet.Org',
#);
#
#my $dal = $irc->newconn(Nick   => 'Fandanta',
#                        Server => 'us.dal.net',
#);
#
#my $open = $irc->newconn(Nick   => 'Fandanta',
#                        Server => 'irc.openprojects.net',
#);
#
##my $newnet = $irc->newconn(Nick   => 'Fandanta',
##                           Server => 'irc.chelmsford.com',
##);
#
#push @irc, $efnet || (), $under || (), $dal || (), $open || (), $newnet || ();

sub on_connect {
    my $self = shift;

    print "Joining #fandanta\n";
    $self->join("#fandanta");

    print "Joining #irkles\n";
    $self->join("#irkles");
}

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

for (@irc) {
    $_->add_global_handler([ 251,252,253,254,302,255 ], \&on_init);
}

# Prints the names of people in a channel when we enter.
sub on_names {
    my ($self, $event) = @_;
    my (@list, $channel) = ($event->args);    # eat yer heart out, mjd!

    # splice() only works on real arrays. Sigh.
    ($channel, @list) = splice @list, 2;

    print "Users on $channel: @list\n";
}

for (@irc) {
    $_->add_global_handler(353, \&on_names);
}

# Yells about incoming CTCP PINGs.
sub on_ping {
    my ($self, $event) = @_;
    my $nick = $event->nick;

    $self->ctcp_reply($nick, join (' ', ($event->args)));
    print "*** CTCP PING request from $nick received\n";
}

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

for (@irc) {
    $_->add_handler('crping', \&on_ping_reply);
}

# Change our nick if someone stole it.
sub on_nick_taken {
    my ($self) = shift;

    $self->nick(substr($self->nick, -1) . substr($self->nick, 0, 8));
}

for (@irc) {
    $_->add_global_handler(433, \&on_nick_taken);
}

# Reconnect to the server when we die.
sub on_disconnect {
        my ($self, $event) = @_;

        print "Disconnected from ", $event->from(), " (",
              ($event->args())[0], "). Attempting to reconnect...\n";
	print "Sleeping";
	foreach (1..10) {
		sleep 1;
		print ".";
	}
	print "\n";
        $self->connect();
}

for (@irc) {
    $_->add_global_handler('disconnect', \&on_disconnect);
}

# Look at the topic for a channel you join.
sub on_topic {
        my ($self, $event) = @_;
        my @args = $event->args();
	my @to = $event->to();

        # Note the use of the same handler sub for different events.

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
			}
		}
        } else {
            print "The topic for $args[1] is \"$args[2]\".\n";
        }
}

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

    return if $arg =~ m/^\<\w+\> /;
    return if $arg =~ m/^\* \w+ /;

    print "$reverse_hosts{$self}!$to[0] <$nick> $arg\n";

    for my $server (@irc) {
	next if $server == $self;
        for my $to (@to) {
            $server->privmsg($to,"<$nick> $arg");
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
            $server->privmsg($to,"* $nick @args");
        }
    }
}

sub private_msg {
    my $self = shift;
    my $event = shift;

    my $nick = $event->nick;
    my @arg = $event->args;
    my $arg = "@arg";


	if($arg =~ m/^[<>]?(\w{1,16})[<>]?\s+(.*)/) {
		my $to = $1;
		$arg = $2;
		print $reverse_hosts{$self}.'!'.($event->to())[0].
			"!$nick -> $to: $arg";
		for my $server (@irc) {
		    next if $server == $self;
		    $server->privmsg($to,">$nick< $arg");
		}
	} else {
		print $reverse_hosts{$self}.'!'.($event->to())[0].
			"!$nick: $arg";
	}
}

sub on_join {
    my $self = shift;
    my $event = shift;

    my $nick = $event->nick;
    return if $nick eq $self->nick;

    my ($channel) = ($event->to)[0];

    my @arg = $event->args;

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

sub on_part {
	my $self = shift;
	my $event = shift;

	print $reverse_hosts{$self}."!".($event->to)[0]." part ".
		$event->nick." ".$event->userhost;

	return if $event->nick eq $self->nick;

	for my $server (@irc) {
		next if $server==$self;
		for my $to ($event->to) {
			$server->privmsg($to,"*** part ".
				$reverse_hosts{$self}."!".($event->to)[0].
				": ".$event->nick." ".$event->userhost);
		}
	}
}


for (@irc) {
	$_->add_handler('public',  \&public_msg);
	$_->add_handler('msg',     \&private_msg);
	$_->add_handler('caction', \&public_action);
	$_->add_handler('join',	\&on_join);
	$_->add_handler('part',	\&on_part);
}

print "starting with ",Net::IRC->VERSION,"\n";
$irc->start;
