#!/usr/bin/perl -w
use strict;

use Net::IRC;

my $irc = Net::IRC->new();

my $efnet = $irc->newconn(Nick   => 'Fandanta',
                          Server => 'irc.east.gblx.net',
);

my $under = $irc->newconn(Nick   => 'Fandanta',
#                          Server => 'lasvegas.nv.us.undernet.org',
#                          Server => 'austin.tx.us.undernet.org',
                          Server => 'Atlanta.GA.US.Undernet.Org',
);

sub on_connect {
    my $self = shift;

    print "Joining #fandanta\n";
    $self->join("#fandanta");
}

$efnet->add_global_handler('376', \&on_connect);
$under->add_global_handler('376', \&on_connect);


# Handles some messages you get when you connect
sub on_init {
    my ($self, $event) = @_;
    my (@args) = ($event->args);
    shift (@args);
    
    print "*** @args\n";
}

$efnet->add_global_handler([ 251,252,253,254,302,255 ], \&on_init);
$under->add_global_handler([ 251,252,253,254,302,255 ], \&on_init);

# What to do when we receive a private PRIVMSG.
sub on_msg {
    my ($self, $event) = @_;
    my ($nick) = $event->nick;

    print "*$nick*  ", ($event->args), "\n";
}

$efnet->add_handler('msg',    \&on_msg);
$under->add_handler('msg',    \&on_msg);

# Prints the names of people in a channel when we enter.
sub on_names {
    my ($self, $event) = @_;
    my (@list, $channel) = ($event->args);    # eat yer heart out, mjd!

    # splice() only works on real arrays. Sigh.
    ($channel, @list) = splice @list, 2;

    print "Users on $channel: @list\n";
}

$efnet->add_global_handler(353, \&on_names);
$under->add_global_handler(353, \&on_names);

# Yells about incoming CTCP PINGs.
sub on_ping {
    my ($self, $event) = @_;
    my $nick = $event->nick;

    $self->ctcp_reply($nick, join (' ', ($event->args)));
    print "*** CTCP PING request from $nick received\n";
}

$efnet->add_handler('cping',  \&on_ping);
$under->add_handler('cping',  \&on_ping);

# Gives lag results for outgoing PINGs.
sub on_ping_reply {
    my ($self, $event) = @_;
    my ($args) = ($event->args)[1];
    my ($nick) = $event->nick;

    $args = time - $args;
    print "*** CTCP PING reply from $nick: $args sec.\n";
}

$efnet->add_handler('crping', \&on_ping_reply);
$under->add_handler('crping', \&on_ping_reply);

# Change our nick if someone stole it.
sub on_nick_taken {
    my ($self) = shift;

    $self->nick(substr($self->nick, -1) . substr($self->nick, 0, 8));
}

$efnet->add_global_handler(433, \&on_nick_taken);
$under->add_global_handler(433, \&on_nick_taken);

# Reconnect to the server when we die.
sub on_disconnect {
        my ($self, $event) = @_;

        print "Disconnected from ", $event->from(), " (",
              ($event->args())[0], "). Attempting to reconnect...\n";
        $self->connect();
}

$efnet->add_global_handler('disconnect', \&on_disconnect);
$under->add_global_handler('disconnect', \&on_disconnect);

# Look at the topic for a channel you join.
sub on_topic {
        my ($self, $event) = @_;
        my @args = $event->args();

        # Note the use of the same handler sub for different events.

        if ($event->type() eq 'notopic') {
            print "No topic set for $args[1].\n";

        # If it's being done _to_ the channel, it's a topic change.
        } elsif ($event->type() eq 'topic' and $event->to()) {
            print "Topic change for ", $event->to(), ": $args[0]\n";

        } else {
            print "The topic for $args[1] is \"$args[2]\".\n";
        }
}

$efnet->add_handler('topic',   \&on_topic);
$under->add_handler('topic',   \&on_topic);

sub efnet_msg {
    my $self = shift;
    my $event = shift;

    my $nick = $event->nick;
    my ($arg) = $event->args;

    if ($self == $under) {
	print "SELF=UNDER\n";
    }
    if ($self == $efnet) {
	print "SELF=EFNET\n";
    }

    print "EFnet: <$nick> $arg\n";

    $under->privmsg('#fandanta',"<$nick> $arg");
}

sub efnet_action {
    my ($self, $event) = @_;
    my ($nick, @args) = ($event->nick, $event->args);

    shift @args;

    print "EFnet: * $nick @args\n";  
    $under->privmsg('#fandanta',"* $nick @args");
}

$efnet->add_handler('public', \&efnet_msg);
$efnet->add_handler('caction', \&efnet_action);

sub under_msg {
    my $self = shift;
    my ($event) = shift;

    my $nick = $event->nick;
    my ($arg) = $event->args;

    if ($self == $under) {
	print "SELF=UNDER\n";
    }
    if ($self == $efnet) {
	print "SELF=EFNET\n";
    }

    print "Under: <$nick> $arg\n";

    $efnet->privmsg('#fandanta',"<$nick> $arg");
}

sub under_action {
    my ($self, $event) = @_;
    my ($nick, @args) = ($event->nick, $event->args);

    shift @args;

    print "Under: * $nick @args\n";  
    $efnet->privmsg('#fandanta',"* $nick @args");
}

$under->add_handler('public', \&under_msg);
$under->add_handler('caction', \&under_action);

print "starting...\n";
$irc->start;
