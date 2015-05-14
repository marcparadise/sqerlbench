
=== Setup ===
* copy priv/tests.config.example to priv/tests.config
* edit root db host/user values.
* You will need to provide a user who can access the
  'postgres' database  with superuser (or at least createdb),
* ensure that your postgres server is listening  over tcp (it should be doing
  this by default on localhost),
* make sure you have a pg_hba.conf entry to permit tcp login for this user
  and for your sqerl.db_user user from localhost (or over the network if
  you're using a separate box
* For best throughput, configure postgres data dir to reside on an SSD.

=== Running ===
The makefile is currently mess, but it does the job.

```$ make

Real Setup instructions coming soon!


