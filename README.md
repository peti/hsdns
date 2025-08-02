hsdns
=====

[![hackage release](https://img.shields.io/hackage/v/hsdns.svg?label=hackage)](http://hackage.haskell.org/package/hsdns)
[![stackage LTS package](http://stackage.org/package/hsdns/badge/lts)](http://stackage.org/lts/package/hsdns)
[![stackage Nightly package](http://stackage.org/package/hsdns/badge/nightly)](http://stackage.org/nightly/package/hsdns)
[![CI Status](https://github.com/peti/hsdns/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/peti/hsdns/actions/workflows/haskell-ci.yml)

## DESCRIPTION

This library provides an asynchronous DNS resolver on top of GNU ADNS
<http://www.chiark.greenend.org.uk/~ian/adns/>. Not all options are supported,
but A, MX, and PTR lookups work nicely. There is also support for retrieving
generic RR types, CNAMEs, and for NSEC zone walking. The library can be
expected to work with fine ADNS 1.4 or later. It might also work with version
ADNS 1.3, but that hasn't been tested.

The example program adns-reverse-lookup.hs demonstrates how the resolver is
used. Given a list of host names on the command line, it performs an A/PTR
double-lookup and checks whether the records are consistent. The output is
printed in the order in which the DNS responses arrive:

> $ ./adns-reverse-lookup cryp.to localhost www.example.com
> OK: localhost <-> 127.0.0.1
> FAIL: cryp.to -> 217.19.183.102 -> ["zuse.cryp.to"]
> OK: www.example.com <-> 192.0.32.10
