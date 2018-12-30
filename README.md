### Abstract Syntax Tree Builder for Delphi
With DelphiAST you can take real Delphi code and get an abstract syntax tree.

### This fork 

This is a fork of https://github.com/RomanYankovsky/DelphiAST which adds:

* Nodes are not repeatedly allocated and freed, instead using an object cache when a new one is created or destroyed (see `TSyntaxNode` in DelphiAST.Classes.pas. The interesting bit is:

```delphi
   class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
   procedure FreeInstance; override;
```

* String interning, using a different technique to the one currently in DelphiAST (written afterwards? The code in this fork dates from 2016.) This is disabled by default currently since the latest DelphiAST has its own technique.

The general idea is to try to prevent memory fragmentation or many allocations and de-allocations when DelphiAST is used constantly. This code is used in the [Parnassus Bookmarks and Navigator plugins](https://parnassus.co/delphi-tools/), which regularly parse the current unit when the user types. That can be many times an hour, even many times a minute. Early versions had users reporting the IDE gave out of memory errors where there was still a lot of free memory, a classic indication of fragmentation. Releases using this code, especially the string interning, solved those bug reports.

#### Copyright
Copyright (c) 2014-2017 Roman Yankovsky (roman@yankovsky.me) et al (these changes are copyright David Millington 2016-2018.)

DelphiAST is released under the Mozilla Public License, v. 2.0

See LICENSE for details.
