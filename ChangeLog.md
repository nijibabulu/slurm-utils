# Changelog for slurm-utils

## Unreleased changes

## 0.2.5.5

- fix priority for default, parsed and preset options

## 0.2.5.4

- do not mkdir for the same directory more than once in mkdiRewrite

## 0.2.5.0

- add the ability to change the shell
- fix bugs with preests

## 0.2.4.0

- Don't process empty tasks
- Emit an error on empty input

## 0.2.3.0

- Set a default name for jobs (temporary change).

## 0.2.2.0

- Support for intermediate files in `tmprewrite`.
- Addition of the `--dependency` option in `slurmtasks`.

## 0.2.1.0

- Support for multiple preset values in `slurmtasks`.
- Fix builtin `himem` prefix.

## 0.2.0.0

- Addition of **preset** feature to `slurmtasks`. Users may choose from the builtin or custom presets, grouped settings.
- Move from `mtl` to `transformers` library

## 0.1.0.0

- Initial version
- `slurmtasks` and `tmprewrite`
