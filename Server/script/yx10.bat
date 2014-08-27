
cd ../config

start "" werl +P 1024000 -smp disable -name yx_10@127.0.0.1 -setcookie yx -boot start_sasl -config yx -pa ../ebin -s yx server_start -extra 127.0.0.1 9010 10
exit

:: erl +P 1024000 -smp disable -name yx_10@127.0.0.1 -setcookie yx -boot start_sasl -config yx -pa ../ebin -s yx server_start -extra 127.0.0.1 9010 10
:: pause


