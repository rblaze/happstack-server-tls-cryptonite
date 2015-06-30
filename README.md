# happstack-server-tls-cryptonite
TLS support for happstack, based on native crypto implementation in tls and cryptonite packages.

Pro:
  * You are not affected by OpenSSL bugs. Goodbye, script kiddies.
  * More portable, no foreign libraries required.
  * Pure Haskell, something to brag about.

Contra:
  * You have all the cryptonite/tls bugs. If your server is under targeted attack, this may make attacker's job easier.
