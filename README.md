# disruption-tracker
[![Build Status](https://travis-ci.org/passy/disruption-tracker.svg?branch=master)](https://travis-ci.org/passy/disruption-tracker)

> Collecting Tube disruption data. Maybe more later.

## Status

If anything works then only by accident. This is, as with most of my project,
just an exploration. But having code in the open so you can point people at it
and ask questions is often quite useful.

## Setup

Run RethinkDB. I recommend Docker for that kinda thing. Remember to write down
the ports.

```bash
$ docker run -P --name rethink1 rethinkdb:2.3
$ docker ps
```

Build this thing.

```bash
$ stack build
```

Initialize some tables, keys and indices.

```bash
$ stack exec disruption-tracker -- --hostname 192.168.99.100 --port 32769 setup
```

If this fails, please manually drop the db for now so we can recreate it:

```javascript
r.dbDrop('test');
r.dbCreate('test');
```

Set up the RethinkDB before?
```
$ docker start rethink1
```

## Deployment

See https://github.com/passy/disruption-bot-deployment.

## Collecting

Now you can start manually collecting. Automatic polling is coming soon.

```
$ stack exec disruption-tracker -- --hostname 192.168.99.100 --port 32769 collect
```
