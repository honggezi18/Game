
cd ../config
erl +P 1024000 -smp disable -name yx_1@127.0.0.1 -setcookie yx -boot start_sasl -config yx -pa ../ebin -s yx server_start -extra 127.0.0.1 9001 1

pause

