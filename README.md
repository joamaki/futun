# Futun - Simple IP-over-UDP tunnel using TUNTAP
         
Futun is a simple IP-over-UDP tunnel for
minimal-setup tunneling. 

It uses the TUN/TAP
<http://en.wikipedia.org/wiki/TUN/TAP> device and
works on GNU/Linux and Mac OS X.

## Compiling

```
cabal configure
cabal build
```

If you're missing dependencies you can install them with
*cabal install*.

The binary will end up in dist/build/futun/futun.
You can do "cabal install" to install the binary to the
Haskell binary directory.

## Use

| futun server [port]
| futun client [hostname] [port] 

Start the server on machine A and client on machine B:

```
root@A: ./futun server 12345
root@B: ./futun client A 12345
```

Setup IP addresses:

```
root@A: ifconfig tun0 192.168.1.1 pointopoint 192.168.1.2
root@B: ifconfig tun0 192.168.1.2 pointopoint 192.168.1.1
```

And if you want you can setup some NAT to get to say A's
network:

```
root@A: iptables -t nat -A POSTROUTING -s 192.168.1.2 -j MASQUERADE
root@A: sysctl -w net.ipv4.conf.all.forwarding = 1

root@B: ip route add 172.16.0.0/16 via 192.168.1.1
```


