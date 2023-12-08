# Revision history for static-ls

## 0.2.0 -- TBD

* Add support for arguments
  * Support user specified hiedb file via argument
  * Support user specified hiefiles path via argument
  * Support user specified hifiles path via argument
  * Support user specified src base directories
  * Add version command
  * Add help command

* Correctly terminate hie file read attempt on bad version header

* Generate static information for tests rather than relying on hard-coded information

* Support reading interface files for haddock comments

## 0.1.0 -- 2023-04-13

* Initial release supporting
  * find references globally when hie files and hiedb are available
  * type on hover when hie files are available
  * find definition locally and globally when hie files and hiedb are available
