/*
 * mkcache - creates the .cache files for the gofish gopher daemon
 * Copyright (C) 2002 Sean MacLennan <seanm@seanm.ca>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with XEmacs; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <ctype.h>
#include <sys/stat.h>
#include <syslog.h>

#include "gofish.h"

int verbose = 0;
int recurse = 0;
int sorttype = 0;

int mmap_cache_size; // needed by config

char *hostname = NULL;
int port = GOPHER_PORT;

/*
 * TODO
 *   - does not handle symbolic links correctly
 *     - does not know if they are dirs or files
 *   - empty dirs are a problem if non-recursive
 *   - needs a config file
 *   - read .names
 *   - read .links
 *   - read .ignore
 *   - if a directory .cache can not be created,
 *     we still get the entry in the upper layer
 *   - mime types
 */


struct extension {
	char *ext;
	char type;
	int binary;
} exts[] = {
	{ "txt",	'0', 0 },
	{ "html",	'h', 0 },
	{ "htm",	'h', 0 },
	{ "gif",	'I', 1 },
	{ "jpg",	'I', 1 },
	{ "png",	'I', 1 },
	{ "jpeg",	'I', 1 },
	{ "gz",		'9', 1 },
	{ "tgz",	'9', 1 },
	{ "tar",	'9', 1 },
	{ "rpm",	'9', 1 },
	{ "zip",	'9', 1 },
	{ "Z",		'9', 1 },
	{ "pdf",	'9', 1 },
	{ "ogg",	'9', 1 },
	{ "mp3",	'9', 1 },
};
#define N_EXTS	(sizeof(exts) / sizeof(struct extension))

struct entry {
	char *name;
	char type;
	char ftype;
};


// If we are already out of memory, we are in real trouble
char *must_strdup(char *str)
{
	char *new = strdup(str);
	if(!new) {
		syslog(LOG_ERR, "read_config: out of memory");
		exit(1);
	}
	return new;
}


// only set if a number specified
void must_strtol(char *str, int *value)
{
	char *end;
	long n = strtol(str, &end, 0);
	if(str != end)
		*value = (int)n;
}


char *must_alloc(int size)
{
	char *mem;

	if((mem = calloc(size, 1)) == NULL) {
		syslog(LOG_ERR, "Out of memory.");
		exit(1);
	}

	return mem;
}



int read_dir(struct entry **entries, char *path, int level);
int output_dir(struct entry *entries, int n, char *path, int level);


/* 0 */
int simple_compare(const void *a, const void *b)
{
	return strcmp(((struct entry *)a)->name, ((struct entry *)b)->name);
}

/* 1 */
int dirs_compare(const void *a, const void *b)
{
	const struct entry *ea = a, *eb = b;

	if(ea->ftype == '1') {
		if(eb->ftype == '1')
			return strcmp(ea->name, eb->name);
		else
			return -1;
	}
	if(eb->ftype == '1')
		return 1;

	return strcmp(ea->name, eb->name);
}

/* 2 */
int dirs_type_compare(const void *a, const void *b)
{
	const struct entry *ea = a, *eb = b;
	int t;

	if(ea->ftype == '1') {
		if(eb->ftype == '1')
			return strcmp(ea->name, eb->name);
		else
			return -1;
	}
	if(eb->ftype == '1')
		return 1;

	if((t = ea->type - eb->type) == 0)
		return strcmp(ea->name, eb->name);
	else
		return t;
}


static void free_entries(struct entry *entries, int nentries)
{
	struct entry *entry;
	int i;

	for(entry = entries, i = 0; i < nentries; ++i, ++entry)
		free(entry->name);
	free(entries);
}


// Returns the number of entries in the .cache file
int process_dir(char *path, int level)
{
	int nfiles;
	struct entry *entries = NULL;

	if(verbose) printf("Processing [%d] %s\n", level, path);

	if((nfiles = read_dir(&entries, path, level)) == 0)
		return 0;

	switch(sorttype) {
	default:
		printf("Unsupported sorttype %d\n", sorttype);
		// fall thru
	case 0:
		qsort(entries, nfiles, sizeof(struct entry), simple_compare);
		break;
	case 1:
		qsort(entries, nfiles, sizeof(struct entry), dirs_compare);
		break;
	case 2:
		qsort(entries, nfiles, sizeof(struct entry), dirs_type_compare);
		break;
	}

	output_dir(entries, nfiles, path, level);

	free_entries(entries, nfiles);

	return nfiles;
}


int output_dir(struct entry *entries, int n, char *path, int level)
{
	FILE *fp;
	char fname[PATH_MAX];
	struct entry *e;
	int i;

	sprintf(fname, "%s/.cache", path);

	if(!(fp = fopen(fname, "w")))
	{
		perror(path ? path : "root");
		return 0;
	}

	for(e = entries, i = 0; i < n; ++i, ++e)
	{
	    if(level == 0)
	    {
		    fprintf(fp, "%c%s\t%s\t%s\t%d\n",
				    e->type, e->name, e->name, hostname, port);
	    }
	    else
	    {
		    fprintf(fp, "%c%s\t%s/%s\t%s\t%d\n",
		    		    e->type, e->name, path, e->name, hostname, port);
	    }
	}

	fclose(fp);

	return n;
}


void add_entry(struct entry **entries, int n, char *name, int isdir)
{
	struct entry *entry;
	char *ext;

	*entries = realloc(*entries, (n + 1) * sizeof(struct entry));
	if(*entries == NULL) {
		printf("Out of memory\n");
		exit(1);
	}

	entry = (*entries) + n;

	entry->name = must_strdup(name);
	if(isdir) {
		entry->type = entry->ftype = '1';
		return;
	}
	else if((ext = strrchr(name, '.'))) {
		int i;
		char *mime;

		++ext;
		for(i = 0; i < N_EXTS; ++i)
			if(strcasecmp(ext, exts[i].ext) == 0) {
				entry->type = exts[i].type;
				entry->ftype = exts[i].binary ? '9' : '0';
				return;
			}

		// If there is an extension, default to binary
		// Most formats are binary.
		entry->type = entry->ftype = '9';
		/*
		if((mime = mime_find(ext)))
		{
			// try to intuit the type from the mime...
			if(strncmp(mime, "text/html", 9) == 0)
			{
				entry->type  = 'h';
				entry->ftype = '0';
			}
			else if(strncmp(mime, "text/", 5) == 0)
				entry->type = entry->ftype = '0';
			else if(strncmp(mime, "image/", 6) == 0)
			{
				entry->type = 'I';
				entry->ftype = '9';
			}
		}
		*/
	}
	else
		// Default to text as per gopher spec
		entry->ftype = entry->type = '0';
}


static int isdir(struct dirent *ent, char *path, int len)
{
	struct stat sbuf;
	char *full;

	// +2 for / and \0
	if(!(full = malloc(len + strlen(ent->d_name) + 2))) {
		printf("Out of memory\n");
		exit(1);
	}
	sprintf(full, "%s/%s", path, ent->d_name);
	if(stat(full, &sbuf)) {
		perror(full);
		exit(1);
	}
	free(full);

	return S_ISDIR(sbuf.st_mode);
}


int read_dir(struct entry **entries, char *path, int level)
{
	DIR *dir;
	struct dirent *ent;
	int nfiles = 0;
	int len = strlen(path);

	if(!(dir = opendir(path))) {
		perror("opendir");
		return 0;
	}

	while((ent = readdir(dir))) {
		if(*ent->d_name == '.') continue;

		if(strcmp(ent->d_name, "gophermap") == 0) continue;

		if(level == 0 && strcmp(ent->d_name, "favicon.ico") == 0)
			continue;

		// Do not add the top level icons directory
		if(level == 0 && strcmp(ent->d_name, "icons") == 0)
			continue;

		if(isdir(ent, path, len)) {
			add_entry(entries, nfiles, ent->d_name, 1);
			++nfiles;

			if(recurse) {
				char *full;

				// note: +2 for / and \0
				if(!(full = malloc(len + strlen(ent->d_name) + 2))) {
					printf("Out of memory\n");
					exit(1);
				}
				if(level == 0)
					strcpy(full, ent->d_name);
				else
					sprintf(full, "%s/%s", path, ent->d_name);
				process_dir(full, level + 1);
				free(full);
			}
			else if(verbose > 1) printf("  %s/\n", ent->d_name);
		} else {
			if(verbose > 1) printf("  %s\n", ent->d_name);
			add_entry(entries, nfiles, ent->d_name, 0);
			++nfiles;
		}
	}

	closedir(dir);

	return nfiles;
}


int main(int argc, char *argv[])
{
	char *dir = NULL;
	char full[PATH_MAX];
	int c;
	int level;

	int option_index = 0;

	while((c = getopt(argc, argv, "c:prs:v:h:")) != -1)
	{
	    switch(c)
	    {
		case 'r':
		    recurse = 1;
		    break;
		case 's':
		    sorttype = strtol(optarg, 0, 0);
		    break;
		case 'v':
		    ++verbose;
		    break;
		case 'h':
		    hostname = strdup(argv[argc - 1]);
		    break;
		default:
		    printf("usage: %s [-rv] [dir]\n", *argv);
		    exit(1);
	    }
	}

	//mime_init();

	if(optind < argc) {
		dir = argv[optind];
		if(!realpath(dir, full)) {
			perror(dir);
			exit(1);
		}
		if(*dir == '/') ++dir;
	}

	if(dir == NULL || *dir == '\0')
		dir = ".";

	if(verbose > 1)
		printf("hostname '%s' port '%s' dir '%s'\n",
			   hostname, port, dir);

	level = strcmp(dir, ".") ? 1 : 0;
	process_dir(dir, level);

	return 0;
}


/* Dummy functions for config */
void set_listen_address(char *addr) {}
void http_set_header(char *fname, int header) {}
