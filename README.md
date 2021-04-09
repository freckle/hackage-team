# Hackage Team

CLI for maintaining a team of maintainers on Hackage packages.

This tool is meant to be run whenever someone joins or leaves your team and you
want to grant or remove their access as an Uploader for all of your Hackage
packages. The idea is to have a shared user who is only a Maintainer (not an
Uploader) and this tool can act as that user to update all of the maintainers of
all of the packages they are added to.

## Usage


1. Create a shared user and have the trustees grant them Maintainer rights

1. Add this user to all of your Hackage packages maintainers lists

1. Generate an API Key for that user and export it as `HACKAGE_API_KEY` when
   running this CLI

1. Run the CLI, passing a list of Hackage usernames as stdin

   ```console
   export HACKAGE_API_KEY=***
   hackage-team <<'EOM'
   OneUser
   SomeOtherUser
   EOM
   ```

1. View the output:

   ```
   TODO
   ```

1. If satisfied, re-run with `--fix` to actually adjust the maintainers lists

## Future ideas

- `--exclude` to not update all packages the team user has access to
- External sources for team list (GitHub Team?)

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
