Haskell Enterprise Logging Platform
===================================

Objectives
----------
HELP's objective is to create a logging platform for the purpose of collecting, archiving, and searching large numbers of (generally) short log entries. Primary goals include:

* Full-text search
* Simple configuration (YAML)
* An external tcp interface
* Fast import from the filesystem
* Small install footprint (a binary install should require at most an installed database)
* A web-based interface suitable for use without technical understanding of the underlying system
* Multi-platform support
* Distributed/clustered capabilities
* Intelligent caching based on previous searches

Purpose
-------
This project intends to remedy the lack of a free/open-source log and search platform capable of processing and searching multiple daily gigabytes of logs, importing old logs from the filesystem in faster than real time, providing clustered log and search capabilities, and providing a simple web-based user interface with real time search capabilities without requiring the installation of large, difficult to install, or poorly maintained dependencies. By providing these features, we intend to create a product usable by small- to medium-sized information technology (IT) departments needing a non-invasive “plug-and-play” logging solution.

It should not be inferred from the above paragraph that there are a lack of free/open-source log and search platforms; rather, there are a lack of log and search platforms meeting the author's specific needs (primarily fast import from the filesystem and ease of use by non-technical users). HELP will [attempt to improve on this issue] (http://xkcd.com/927/).

Schedule
--------
The primary development cycle for this project is bounded by summer semester, 2013. Development will likely continue after August, 2013.