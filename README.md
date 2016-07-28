## HDO: A HAskell Digital Ocean Client ##

[![Build Status](https://travis-ci.org/capital-match/hdo.svg?branch=master)](https://travis-ci.org/capital-match/hdo)

**WARNING** This implementation still covers only a small subset of DO API

This is a [Digital Ocean](https://www.digitalocean.com/) client written in [Haskell](http://haskell.org). It can be used either as a
library or as command-line utility.


## Compile
`stack setup`
`stack build`

## Usage
First export the AUTH_TOKEN:
`export AUTH_TOKEN=2342342341234eaf`

List existing droplets:
`stack exec docean -- droplets list`

List existing regions:
`stack exec docean -- regions list`

## Implemented commands:
```
droplets create
droplets destroy <dropletId>
droplets list
droplets power_off <dropletId>
droplets power_on  <dropletId>
droplets snapshot  <dropletId> <snapshotName>
droplets action    <dropletId> <actionId>
droplets <dropletId> snapshots
droplets <dropletId>
droplets ssh <dropletId or Name>
images list
regions list
keys list
sizes list
```
