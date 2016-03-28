#!/bin/bash -eux
ROOT=$(dirname $(readlink -f $0))
set -o pipefail

HW_IF=wlxb8a386789663
IF=wlan0_ap
IP=192.168.149.1
NET=192.168.149.0/24

IF_UPSTREAM=tun1
MARK_UPSTREAM=1
TABLE_UPSTREAM=10
GW_UPSTREAM=10.20.30.5

start-interfaces() {
    nmcli d set $HW_IF managed no
    iw dev $HW_IF interface add $IF type __ap
    nmcli d set wlan0_ap managed no
    ip link set dev $IF address 12:34:56:78:ab:ce
    ifconfig $IF up 192.168.149.1 netmask 255.255.255.0
}

start-iptables() {
    iptables-save | grep -v "ap=$IF" | iptables-restore

    iptables -m comment --comment "ap=$IF" -t nat -A POSTROUTING -o $IF_UPSTREAM -s $NET -j MASQUERADE

    iptables -m comment --comment "ap=$IF" -t mangle -A PREROUTING -j CONNMARK --restore-mark
    iptables -m comment --comment "ap=$IF" -t mangle -A PREROUTING -m mark ! --mark 0 -j RETURN # if already set, we're done
    iptables -m comment --comment "ap=$IF" -t mangle -A PREROUTING -i $IF_UPSTREAM -j MARK --set-mark $MARK_UPSTREAM

    iptables -m comment --comment "ap=$IF" -t mangle -A POSTROUTING -o $IF_UPSTREAM -j MARK --set-mark $MARK_UPSTREAM
    iptables -m comment --comment "ap=$IF" -t mangle -A POSTROUTING -j CONNMARK --save-mark
}


start-routing() {
    while ip rule del prio 1501 &> /dev/null; do
        :
    done
    while ip rule del prio 1500 &> /dev/null; do
        :
    done
    ip rule add from $IP/32 pref 1500 lookup main
    ip rule add from $NET pref 1501 lookup $TABLE_UPSTREAM
    ip rule add fwmark $MARK_UPSTREAM pref 1501 lookup $TABLE_UPSTREAM

    ip route flush table $TABLE_UPSTREAM

    ip route add table $TABLE_UPSTREAM $NET dev $IF proto kernel scope link src $IP
    ip route add table $TABLE_UPSTREAM default via $GW_UPSTREAM dev $IF_UPSTREAM
}

start-interfaces
start-routing
start-routing

# iptables -t nat -A POSTROUTING -s 10.20.30.0/24 -o eth0 -j MASQUERADE
# iptables -A FORWARD -i tun0 -s 10.20.30.0/24 -j ACCEPT
# iptables -A FORWARD -o tun0 -d 10.20.30.0/24 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
