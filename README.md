Futun - Simple UDP-based tunneling software on top of TUN/TAP.

Compiling
=========

Make sure you got Haskell installed (apt-get install haskell-platform).
Also install cabal: apt-get install cabal-install.
First install the tuntap library:
$ cd tuntap
$ cabal configure && cabal build && cabal install

If it complains about missing dependencies do a 
cabal install <dependency> for those and repeat.

Then compiling futun:
$ cd ../ ; cabal configure && cabal build && cabal install

The binary will end up in dist/build/futun/futun.
You can do "cabal install" to install the binary to the
Haskell binary directory.

Running
=======

Start the server on machine A and client on machine B:

root@A: ./futun server 12345

root@B: ./futun client A 12345

Setup IP addresses:

root@A: ifconfig tun0 192.168.1.1 pointopoint 192.168.1.2
root@B: ifconfig tun0 192.168.1.2 pointopoint 192.168.1.1

And if you want you can setup some NAT to get to say A's
network:

root@A: iptables -t nat -A POSTROUTING -s 192.168.1.2 -j MASQUERADE
root@A: sysctl -w net.ipv4.conf.all.forwarding = 1

root@B: ip route add 172.16.0.0/16 via 192.168.1.1

