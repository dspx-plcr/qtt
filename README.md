# WARNING

You should not use this program, since it will eat all your data if it fails :)

---

# QTT

This program is a very simple implementation of the SuperMemo2 algorithm for
spaced repetition. It is implemented in Idris2 as an experiment in using
quantitative type theory. In order to facilitate the use of linear types, I
added some arbitrary restrictions around the semantics of the program. Please
don't view this as serious code to be run, it has sharp edges that you will cut
yourself on.

You can watch me struggle to build this on [youtube][#yt].

## Building and Running

You'll need a copy of Idris2, after which the program can be built and run with

```sh
idris2 -x main -p linear QTT.idr
```

## Database Format

Cards begin with front matter (free text not containing any special lines
described below), a line of three dashes, then back matter (as in front matter).

A card can contain optional statistical information related to the SuperMemo2
algorithm, and this program will automatically add the information after being
run. If a card contains such information, the backmater is then followed by a
line of three equals signs, then a line of comma seperated values representing
the statistics. This information is managed automatically by the program and
should simply be ignored.

Cards are seperated by two blank lines.

Special lines are used to seperate fields or cards by the program. Such lines
are

```
---
```
```
===
```
```


```

This program was only tested on OpenBSD, so I don't know how different line
terminators affects the running of the program.

[yt]: https://www.youtube.com/playlist?list=PL5CIEWYHtEXommjLd_zOPgLVyQCdTEYfu
