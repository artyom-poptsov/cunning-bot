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

SCM_BOT_SOURCES = \
	bot.scm			\
	commands.scm		\
	plugins.scm		\
	run-cbot.scm

pkgguilesitedir = $(guilesitedir)/cunning-bot
nobase_dist_pkgguilesite_DATA = $(SCM_BOT_SOURCES)

ETAGS_ARGS = $(SCM_BOT_SOURCES)

EXTRA_DIST = TODO.org docs/plugins.org

SUBDIRS = plugins

## Makefile.am ends here
