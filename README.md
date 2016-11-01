## HDO: A HAskell Digital Ocean Client ##

[![Build Status](https://travis-ci.org/capital-match/hdo.svg?branch=master)](https://travis-ci.org/capital-match/hdo)

This is a [Digital Ocean](https://www.digitalocean.com/) client written in [Haskell](http://haskell.org). It can be used either as a
library or as command-line utility.

# Build

*hdo* is built with [stack](https://github.com/commercialhaskell/stack/), using [Stackage LTS-7.7](https://www.stackage.org/lts-7.7) dependencies. To build, follow the standard instructions:

Install the correct GHC version:

> stack setup 

Build and install the package locally, possibly making executable `docean` available from `PATH`:

> stack install

# Usage

## Command-line Tool 

Command-line tool is named `docean`. If it's been installed, it should be available in your PATH. Otherwise, you can run it from the source directory using `stack exec docean -- <args>`. `docean` expects authentication token to be available from environment variable `AUTH_TOKEN`, see [Digital Ocean API](https://developers.digitalocean.com/documentation/v2/#authentication) for details on how to get that authentication token.

> export AUTH_TOKEN=2342342341234eaf

## Available Commands

**NOTE**: Not all the available APIs from DO are implemented (yet).

### Droplets

#### Creating a Droplet

This is the most complex command, as creating a droplet can use a lot of different options.

```
$ docean droplets create -r ams2 -s 1gb -i ubuntu-16-04-x64 -n deploy-test -k '[2436510]'
waiting for droplet deploy-test to become Active: 60s
waiting for droplet deploy-test to become Active: 59s
waiting for droplet deploy-test to become Active: 58s
waiting for droplet deploy-test to become Active: 57s
waiting for droplet deploy-test to become Active: 56s
waiting for droplet deploy-test to become Active: 55s
waiting for droplet deploy-test to become Active: 54s
waiting for droplet deploy-test to become Active: 53s
waiting for droplet deploy-test to become Active: 52s
30876135
     deploy-test [Active] ams2: Amsterdam 2
     1024M/30G/1 cores
     IPv4
       95.85.44.239/255.255.255.0 [Public]
     IPv6
```

The available parameters are:

* `-r|--region`: Sets the region where droplet is created, using the *region slug*
* `-s|--size`: Sets the size of the droplet, from `512mb` to `96gb`
* `-n|--name`: Name of the newly created droplet
* `-i|--image`: Slug of the image to use as base image for droplet
* `-k|--keys`: List (in the form readable by Haskell to convert to type `[Int]`) of keys to add for accessing the droplet
* `-b|--background`: Create droplet in the background, returning only the identifier of action to be checked later. If not set, creation is *synchronous*: The client will check wait for droplet to be up for 60s.

When successfully created, the main characteristics of the droplet are printed to the console.

#### Other Droplet Operations

Destroy an existing droplet:

```
$ droplets destroy 30876135
-
```

List all created droplets:

```
$ docean droplets list
21002433
     droplet-01 [Active] sgp1: Singapore 1
     1024M/30G/1 cores
     IPv4
       1.2.3.4/255.255.240.0 [Public]
     IPv6
26082279
     droplet-02 [Active] sgp1: Singapore 1
     512M/20G/1 cores
     IPv4
       1.2.3.5/255.255.240.0 [Public]
     IPv6
31114780
     droplet-03 [Active] ams2: Amsterdam 2
     2048M/40G/2 cores
     IPv4
       5.6.7.8/255.255.240.0 [Public]
     IPv6
```

Shutdown/startup a droplet:

```
$ docean droplets power_off 31166171
[163908062] 2016-11-01 21:08:46 UTC -> -
     31166171 PowerOff: InProgress
```

```
$ docean droplets power_on 31166171
[163908350] 2016-11-01 21:10:16 UTC -> -
     31166171 PowerOn: InProgress
```

Retrieve status of an action initiated on the droplet (usually related to power on/off or snapshots):

```
$ docean droplets action 31166171 163908062
[163908062] 2016-11-01 21:08:46 UTC -> 2016-11-01 21:08:58 UTC
     31166171 PowerOff: Completed
```

Take a snapshot of an existing droplet (must be powered off):

```
$ docean droplets snapshot 31166171 mysnapshot
[163908530] 2016-11-01 21:11:19 UTC -> -
     31166171 MakeSnapshot: InProgress
```

List existing snapshots for droplet:

```
$ docean droplets 31166171 snapshots
20648640 mysnapshot Ubuntu
```

Connect to the droplet through SSH (can be given a name or a droplet ID):

```
$ docean droplets ssh 31166171
The authenticity of host '1.2.3.4 (1.2.3.4)' can't be established.
RSA key fingerprint is 3f:16:4f:f4:25:85:85:77:1e:90:bd:8c:6b:90:48:eb.
Are you sure you want to continue connecting (yes/no)? yes
Warning: Permanently added '1.2.3.4' (RSA) to the list of known hosts.
Welcome to Ubuntu 16.04.1 LTS (GNU/Linux 4.4.0-45-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage

0 packages can be updated.
0 updates are security updates.



The programs included with the Ubuntu system are free software;
the exact distribution terms for each program are described in the
individual files in /usr/share/doc/*/copyright.

Ubuntu comes with ABSOLUTELY NO WARRANTY, to the extent permitted by
applicable law.

root@droplet:~# 
```

### Images

### Regions

### Keys

### Sizes

### Floating IPs

### Domains

## Library

TBD
