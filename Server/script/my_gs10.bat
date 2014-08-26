


cd ../config
erl +P 1024000 -smp disable -name yxyz10@192.168.4.20 -setcookie gs -boot start_sasl -config gs -pa ../ebin -s gs start -extra 192.168.4.20 9210 10

pause
