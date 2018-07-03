# OneWaySynchronize
Please see http://mmmunk.dk/onewaysynchronize/

## Usage
```
OneWaySynchronize 0.9
Fast file backup and mirroring
Copyright (C) 2008-2015 Thomas Munk
http://mmmunk.dk/onewaysynchronize

--- Basic usage ---

OneWaySynchronize.exe Source Destination [Options]

- Source and Destination must be fully qualified directory paths
- Source will never be written to or changed in any way
- Destination will be modified in every possible way to match Source (WARNING)

--- Options ---

Inclusion/exclusion of directories/files. Wildcards allowed. Excludes come
before includes. Defaults are to include everything and exclude nothing:

/ID:name[;name]  List of directory names to include only
/IF:name[;name]  List of file names to include only
/ED:name[;name]  List of directory names to not include
/EF:name[;name]  List of file names to not include

Disabling of standard behaviors:

/NA          No reset of file attributes
/ND          No delete of directories/files
/NW          No overwrite of existing files
/TD:seconds  Maximum difference for timestamps to be considered equal
             Default is 2 seconds (FAT time resolution)

Logging:

/LQ           Quiet. Log only errors to display (stdout)
/LO:filename  Log to file. Overwrite existing file
/LA:filename  Log to file. Append to existing file
              To have fx a monthly file: /LA:%date:~6,4%-%date:~3,2%.log
```
