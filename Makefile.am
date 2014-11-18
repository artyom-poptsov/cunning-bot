## Config file for GNU Automake. 
##
## Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Cunning Bot, an IRC bot written in Guile
## Scheme.
## 
## Cunning-Bot is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Cunning-Bot is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Cunning-Bot.  If not, see <http://www.gnu.org/licenses/>.

bin_SCRIPTS = \
	tappet

SOURCES = \
	bot.scm				\
	commands.scm			\
	plugins.scm			\
	run-cbot.scm			\
	log.scm

# XXX: Ship part of spells collection of Scheme libraries to simplify
# the installation.
# See <http://community.schemewiki.org/?spells>
SOURCES += \
	spells/network.scm		\
	spells/network/compat.scm	\
	spells/network/utils.scm

pkgguilesitedir = $(guilesitedir)/cunning-bot
nobase_dist_pkgguilesite_DATA = $(SOURCES)

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'

EXTRA_DIST = \
	tappet.in

SUFFIXES = .in

.in:
	$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

ETAGS_ARGS = $(SOURCES)

EXTRA_DIST = TODO.org docs/plugins.org

CLEANFILES = $(bin_SCRIPTS)

SUBDIRS = plugins

## Makefile.am ends here
