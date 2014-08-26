cd ../
rem del ebin\*.beam
rem erl -noshell -s make all -s init stop
cd config
erl +P 1024000 +K true -smp disable -name yxyz1@192.168.10.17 -setcookie gs -boot start_sasl -config gs -pa ../ebin -s gs start -extra 192.168.10.17 9801 1
