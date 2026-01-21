# Revision history for static-ls

## Unreleased -- 2025-07-06
  * New code actions/quick fixes
    * Add required extension
    * Insert associated type
    * Insert cases
    * Insert fields
    * Insert missing methods
    * Use valid hole fit

## 1.0.0 -- 2024-09-24
  * Re-architect to use tree-sitter and in memory representation of file system
  * New features:
    * Auto-import code action
    * Auto-completion
    * Fly-imports on auto-completion
    * Rename
    * support for persistent files
    * Diagnostics
    * Go-to implementation
  * Show all types for go to type def when ambiguous
  * Use lexed diffing algorithm for position resolution on stale hiefiles

## 0.2.0 -- 2024-02-04

* Add support for arguments
  * Support user specified hiedb file via argument
  * Support user specified hiefiles path via argument
  * Support user specified hifiles path via argument
  * Support user specified src base directories
  * Add version command
  * Add help command

* Bugfixes
  * Correctly terminate hie file read attempt on bad version header
  * Generate static information for tests rather than relying on hard-coded information

* Support reading interface files for haddock comments (https://github.com/josephsumabat/static-ls/pull/30)

* Support goto typedef (https://github.com/josephsumabat/static-ls/pull/31)

* Support workspace symbol (https://github.com/josephsumabat/static-ls/pull/18)

## 0.1.0 -- 2023-04-13

* Initial release supporting
  * find references globally when hie files and hiedb are available
  * type on hover when hie files are available
  * find definition locally and globally when hie files and hiedb are available
