# denvi
vimscript for configuration
lua for scripting

- Why Lua <https://web.archive.org/web/20150219224654/https://blog.datamules.com/blog/2012/01/30/why-lua/>
- The Design of Lua <https://cacm.acm.org/magazines/2018/11/232214-a-look-at-the-design-of-lua/fulltext>
- Scripting architecture considerations <http://oldblog.antirez.com/post/redis-and-scripting.html>
- LuaJIT performance <https://julialang.org/benchmarks/>
- Discussion of JavaScript vs Lua <https://github.com/vim/vim/pull/5198#issuecomment-554693754>
- Discussion Python embedding <https://lobste.rs/s/pnuak4/mercurial_s_journey_reflections_on#c_zshdwy>

## prog-mode
tune words :h word-motions
### source nav
Jump to local - using Telescope as fuzzy viewer, query tree-sitter and preview / jump-to

### Status bar
- [ ] show the file full path
- [ ] show background tasks (there is ALE cargo task at least, :tags scanning)


### j-style navigation for envi
`C-]` j to subj
`C-0` j back
`C-t` j back (returns back after jumping to :h x)
`gO` table of contents (used in help filetype). Maybe this one can be re-used as go to import or local file definition.

### Bindings
C-c do not fire `au InsertLeave`
C-d in normal mode is free to use 

### Benchmarks
```sh
time nvim --startuptime startup.log -c exit
```

# rel/path/to/
Usually there are few locations (nested implied):

Project (git enabled, has git-root)
Local files (like ~/tmp/)
System sources (homebrew, includes and so on)

## Titles
Project files: project rel/path/to/file

... .zshrc

Local files: full/path/to/file (abbrev: ~, ...)

System sources: bundle name? TODO


## Tricks to remember
`C-f` when `:` or `/` is active
