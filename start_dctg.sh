cd /home/ubuntu/dctg
erl -setcookie test -sname controller@testercontroller \
-pa /home/ubuntu/dctg/apps/dctg/ebin/ \
-pa /home/ubuntu/dctg/apps/dctg_worker/ebin/ \
-pa /home/ubuntu/dctg/apps/dctg_web/ebin/ \
-pa /home/ubuntu/dctg/deps/mochiweb/ebin/ \
-pa /home/ubuntu/dctg/deps/emysql/ebin/ \
-pa /home/ubuntu/dctg/deps/procket/ebin/ \
-s dctg_web \
-s dctg -rsh ssh

