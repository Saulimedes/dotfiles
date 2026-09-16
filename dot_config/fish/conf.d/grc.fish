if not type -q grc; or not status is-interactive
    exit
end

set -l cmds \
    as ant blkid cc configure curl cvs df diff dig dnf docker docker-compose \
    du env fdisk findmnt free g++ gas gcc getfacl getsebool gmake id ifconfig \
    iostat ip iptables iwconfig journalctl kubectl last ld lsattr lsblk lsmod \
    lsof lspci make mount mtr netstat nmap ntpdate php ping ping6 ps sar \
    semanage sensors showmount sockstat ss stat sysctl tcpdump traceroute \
    traceroute6 tune2fs ulimit uptime vmstat wdiff whois

for cmd in $cmds
    if type -q $cmd
        alias $cmd "grc --colour=auto $cmd"
    end
end
