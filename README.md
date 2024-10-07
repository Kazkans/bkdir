# bkdir - Bookmark manager written in chicken scheme

## Compiling:
`csc ./bkdir.scm`

## Usage:
`bkdir` - runs bookmark manager and after selecting item it outputs to stdout.

`bkdir b` - saves current dir to bookmarks

Directory where are saved bookmark is `$XDH_HOME/bkdir/bookmarks` or when `$XDG_HOME` is not set `$HOME/.loca/share/bkdir/bookmarks`

It uses chicken C FFI to use termbox2 for TUI.
