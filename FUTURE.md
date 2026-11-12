# Future work

## Update only the changed rows of a list

### Problem

When one row of a list changes, `ek9-list--render` prints all rows again.
For a list of 1000 pods, `tabulated-list-print` takes approximately 80 ms.
A large list can update every 3 seconds (the AGE column), and also at each watch event.

### Measurements (1000 rows, 10 columns, with faces)

| Step | Time |
|---|---|
| Compare the new rows with the shown rows | 0.2 ms |
| Build the rows from the items | 3.7 ms |
| Print all rows | 80 ms |

The compare and the build are fast.
The print of all rows is the slow step.

### Proposal

Replace only the rows that changed.
Keep the other rows in the buffer.

1. Compare the new entries with the shown entries for each row ID.
2. For a changed row, delete the line and insert the new line.
3. Delete the lines of the rows that went away.
4. Insert the new rows at their sort positions.
5. Print all rows only if the columns, the widths, the sort, or the filter changed.

### Constraints

- Do not use the UPDATE argument of `tabulated-list-print`.
  It keeps a row with the same ID, even when its text or its face changed.
  See the comment in `ek9-list--render`.
- Keep point, the window points, and the mark on the same row.
- Keep `tabulated-list-entries` correct, because other code reads it.
