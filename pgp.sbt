// https://github.com/sbt/sbt-pgp/issues/126
PgpKeys.pgpSecretRing in Global := file("~/.gnupg/pubring.kbx")
