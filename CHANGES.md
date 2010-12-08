# couch_planet CHANGES

## Version 1.1.0

### Crawler:

* Add shell script `start-dev.sh` for development purpose. Starts the crawler
  in interactive mode.
* More tolerant feed parsing:
    - allow (custom) attributes for any of a feed's XML elements
    - ensure that everything is always properly escaped (and hence valid JSON).
* Support HTTP over SSL.

### CouchApp:

* Support continuous scrolling.
* Refined CSS.
