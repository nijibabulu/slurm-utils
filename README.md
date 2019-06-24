# Slurm Utilities

This package provides a pair of utilities for submitting jobs to a SLURM cluster:
  - [`slurmtasks`](#slurmtasks) - creates a SLURM script from a file (or `stdin`) containing a list of tasks, one per line. 
  - [`tmprewrite`](#tmprewrite) - rewrites a list of tasks to be executed in the temporary directory, avoiding IO overload on NFS.
  
The default behavior has been designed and maintained with the [LiSC](http://cube.univie.ac.at/lisc) cluster in mind, but are general tools for generating scripts for a slurm-based workload manager.
  
Together, they form a pipeline which can be used to easily submit jobs to a slurm cluster. Here's an example submitting a simple task, converting it to a slurm-formatted script, and submitting it to the cluster:

```
$ echo "gzip -c smallfile.txt > smallfile.txt.gz" | slurmtasks | sbatch
Submitted batch job 580886
```

Execute your job in temporary space only:

```
$ echo "gzip -c {largefile.txt:i} > {largefile.txt.gz:o}" | tmprewrite | slurmtasks | sbatch
Submitted batch job 580887
```

Submit multiple tasks:

```
$ for f in dir_with_largefiles; do echo "gzip -c {$f:i} > {$f.gz:o}"; done | tmprewrite | slurmtasks | sbatch
Submitted batch job 580888
```

For details, see `slurmtasks --help` or `tmprewrite --help` or see below.

## Installing

For users of LiSC, current versions executables are available in `/proj/rpz/slurmbin/`. For everyone else, clone this repository and, if you haven't already, install [stack](https://docs.haskellstack.org/en/stable/README/). In the source directory, run 

```
stack install
```
  
## slurmtasks

This utility creates a slurm script out of a list of tasks that can in turn be submitted via `sbatch`. Take the following example:

**tasks.txt**

```
echo "task1"
echo "task2"
```

Running slurmtasks with this file as input 
```
$ slurmtasks tasks.txt
#!/bin/bash
#
#SBATCH --output=./%x.o%A.%a
#SBATCH --error=./%x.e%A.%a
#SBATCH --cpus-per-task=1
#SBATCH --workdir=.
#SBATCH --array=1-2
#SBATCH --mem=3
#SBATCH --nice=0
case ${SLURM_ARRAY_TASK_ID} in
    1)
        echo "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on ${HOSTNAME} at $(date)"
        echo "  <Command>"
        echo "echo "task1""
        echo "  </Command>"
        echo "task1"
        if  [  $? -ne 0  ] ; then
            echo "Your program exited with error $?"
        fi
        echo "Job ${SLURM_JOB_ID}.${SLURM_ARRAY_TASK_ID} finished on ${HOSTNAME} at $(date)"
    ;;
    2)
...
        echo "task2"
...
    ;;
esac
```

`slurmtasks` provides an straightforward interface to modifying the header and the nature of the task list. For example, to group tasks into sets of 5 each, you might do the following:

```
$ for i in $(seq 6); do echo "task$i"; done | slurmtasks --group-by 3
#!/bin/bash
...
case ${SLURM_ARRAY_TASK_ID} in
    1)
...
        echo task1
        echo task2
        echo task3
...
    ;;
    2)
...
        echo task4
        echo task5
        echo task6
...
    ;;
esac
```


### Presets

Many SLURM options go hand in hand, for instance an increase in the number of cores requested often implies that more memory should be requested. Presets make it easy to specify these changes together. You may use the builtin presets:

```
Available presets:
  8core: --mem 64 --cpus 8 --features array-8core
  himem: --mem 1000 --cpus 40 --features ''
```

and specify your own in `$HOME/.config/slurm-utils/slurmtasks.yml`. The format of this file should be as above, with one preset per line, a colon separating the name of the preset and the options it entails. You may also tweak the preset options by overriding them on the command line:

```
$ echo "my_8core_memoryhog.sh" | slurmtasks --preset 8core --mem 100
# ...
#SBATCH --cpus-per-task=8          # (from 8core preset: --cpus 8)
#SBATCH --array=1-1
#SBATCH --mem=100000               # (from 8core preset: --mem 64 -> overridden: --mem 100)
# ...
#SBATCH --constraint=array-8core   # (from 8core preset: --features array-8core)
```

### Error checking

Additionally, `slurmtasks` will check for the existence of the specified output and working directories, to protect against submitting a job which cannot be run:

```
$ echo "echo task1" | slurmtasks --workdir NON_EXISTENT_WORKING_DIR --logdir NON_EXISTENT_LOGGING_DIR
slurmtasks:
log directory NON_EXISTENT_LOGGING_DIR does not exist
working directory NON_EXISTENT_WORKING_DIR does not exist

Use --ignore-errors to suppress errors
```

### Job Names

By default, `slurmtasks` does not specify a job name for your script. `sbatch` will automatically name your job after the name of your script:

```
$ echo "sleep 10" | slurmtasks > sleepy.slurm
$ sbatch sleepy.slurm
Submitted batch job 5808989
$ squeue -u rpz
           JOBID       PARTITION     NAME     USER ...
       5808989_1           basic sleepy.s      rpz ...
```

Without a an intermediate script, though, your job will get the rather undignified name of `(null)`, but appear as `sbatch` in the queue:

```
$ echo "sleep 10" | slurmtasks | sbatch 
Submitted batch job 5808989
$ squeue -u rpz
           JOBID       PARTITION     NAME     USER ...
       5808989_1           basic   sbatch      rpz ...
$ ls
... (null).o5808989.1    (null).e5808989.1 ...
```

To fix this problem, you can specify a job name either via `slurmtasks` or via `sbatch`:

```
$ echo "sleep 10" | slurmtasks --name sleepy | sbatch
$ echo "sleep 10" | slurmtasks | sbatch --job-name sleepy
```

both do basically the same thing.

### Large Jobs

With extremely large jobs, the `sbatch` command will choke with some uninformative error messages, such as:

```
sbatch: error: Batch job submission failed: Zero Bytes were transmitted or received
```

In these cases, it may be useful to split up your task list into several different jobs. Alternatively, you can slim down the script that `slurmtasks` outputs. By default, `echo` commands are inserted into every task to make the slurm output files more informative. However, this takes up a lot of extra per-task space. To remove this, use the `--short-tasks` option:

```
$ echo "echo hi" | stack exec slurmtasks  -- --name test --short-tasks
#!/bin/bash
#
#SBATCH --output=./%x.o%A.%a
...
case ${SLURM_ARRAY_TASK_ID} in
    1)
        ulimit -v 3381000
        echo hi
    ;;
esac
```

For more information, run `slurmtasks --help`

## tmprewrite

`tmprewrite` makes the process of turning commands from a pipeline into a straightforward, less error-prone task. It also works well with `slurmtasks`. One just needs to decorate the names of files to transfer with braces and an indicator of which kind:

- `{infile:i}` indicates `infile` is an input file.
- `{outfile:o}` indicates `outfile` is an output file to be created by the command.
- `{dir:d}` indcates `dir` is an output directory that should be present prior to running the command. It will be created.

For example,

```
$ tmprewrite "gzip -c {largefile.txt:i} > {largefile.txt.gz:o}"
set -e ; (flock -w 7200 200 ; hostname > LOCK-host ; cp -Lr largefile.txt $TMPDIR) 200> LOCK ; gzip -c $TMPDIR/largefile.txt > $TMPDIR/largefile.txt.gz ; cp -Lr $TMPDIR/largefile.txt.gz .
```

The command is rewritten with several phases

1. Create a directory if needed,
1. Copy the input files to temporary space,
1. Run the command in temporary space and
1. Copy the output files back to their destination.

`tmprewrite` can also be run without the file lock in the `nolock` mode, and also in `test` mode, which essentially removes the decorators from the filenames

```
$ tmprewrite --mode nolock "gzip -c {largefile.txt:i} > {largefile.txt.gz:o}"
cp -Lr largefile.txt $TMPDIR ; gzip -c $TMPDIR/largefile.txt > $TMPDIR/largefile.txt.gz ; cp -Lr $TMPDIR/largefile.txt.gz .
$ tmprewrite --mode test "gzip -c {largefile.txt:i} > {largefile.txt.gz:o}"
gzip -c largefile.txt > largefile.txt.gz
```

The latter can be useful for testing your command directly in interactive mode before submitting to the cluster.

### Blast databases and genome indices

Blast databases consist of multiple files. They are specified as a base name for a collection of files, and the base name is not a file at all. That means that `cp path/to/blast/db/dbprefix $TMPDIR` will not work, since `dbprefix` is not a file at all. One work around is to include the prefix as the decorated input file:

```
$ tmprewrite --mode nolock  "blastp -query query.fa -db {blast_index:i}/db -outfmt 6 > blast_hits.txt"
cp -Lr blast_index $TMPDIR ; blastp -query query.fa -db $TMPDIR/blast_index/db -outfmt 6 > blast_hits.txt
```

Similarly, the output files of blast indices from `makeblastdb` are specified only as a base filename on the command line. For this we can directly use a directory output file:

```
$ tmprewrite --mode nolock  "makeblastdb -out {blast_index:d}/db -type nucl -in proteins.fa"
mkdir -p $TMPDIR/blast_index ; makeblastdb -out $TMPDIR/blast_index/db -type nucl -in proteins.fa ; cp -Lr $TMPDIR/blast_index .
```

### Error checking

`tmprewrite` checks that input files and output destinations are valid and writes error messages to `stderr` if it looks like you're not going to be able to run the commands:

```
$ tmprewrite "cp {NON_EXISTENT_FILE:i} {NON_EXISTENT_DIR/dest:d}/output"tmprewrite:
File does not exist: NON_EXISTENT_FILE
Directory does not exist: NON_EXISTENT_DIR

Fix these errors or use --ignore-errors to suppress them

$ tmprewrite "cp {EXISTING_FILE:i} {EXISTING_FILE:o}"
tmprewrite:
Destination is an existing filesystem object: EXISTING_FILE

Fix these errors or use --ignore-errors to suppress them
```
