# `command -q`, not `type -q`: forgit defines a `grc` (git revert commit) alias
if not command -q grc; or not status is-interactive
    exit
end

set -l cmds \
    as ant blkid cc configure curl cvs df diff dig dnf docker docker-compose \
    docker-machine du env fdisk findmnt free g++ gas gcc getfacl getsebool \
    gmake id ifconfig iostat ip iptables iwconfig journalctl kubectl last ld \
    ldap lolcat lsattr lsblk lsmod lsof lspci make mount mtr mvn netstat nmap \
    ntpdate php ping ping6 proftpd ps sar semanage sensors showmount sockstat \
    ss stat sysctl systemctl tcpdump traceroute traceroute6 tune2fs ulimit \
    uptime vmstat wdiff whois

for cmd in $cmds
    if type -q $cmd
        alias $cmd "grc --colour=auto $cmd"
    end
end
