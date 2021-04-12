# Hackage Team

CLI for maintaining a team of maintainers on Hackage packages.

## Operation

This tool iterates over all packages it can access with the given token. It
compares the list of actual Maintainers for each package to a central list of
expected maintainers. It reports which users should be added or removed to make
the actual list align with expected. Optionally, it can proceed with adding and
removing as necessary.

## Context

The Hackage Trustees recommend a shared account that is a Maintainer (but not
Uploader) on all of your team packages. Then, this account can be used to
add/remove individual accounts as Uploaders when folks come and go on your team.

This tool can function when run as any user on the team, but it compliments the
_Shared Maintainer_ design described above when used with a token for such a
user.

## Usage

1. Create a shared user and have the trustees grant them Maintainer rights

1. Add this user to all of your Hackage packages' maintainers lists

1. Generate an API Key for that user and export it as `HACKAGE_API_KEY` when
   running this CLI

1. Run the CLI, passing a list of Hackage usernames as stdin

   ```console
   % hackage-team --no-remove < ./FRECKLE_MAINTAINERS.txt
   [Info] Checking package: graphula
   [Info] Expected, not present: halogenandtoast
   [Info] Expected, not present: mjgpy3
   [Info] Checking package: hspec-junit-formatter
   [Info] Expected, not present: cbeav
   [Info] Expected, not present: cdparks
   [Info] Expected, not present: dukerutledge
   [Info] Expected, not present: halogenandtoast
   [Info] Checking package: nonempty-zipper
   [Info] Checking package: sendgrid-v3
   [Info] Expected, not present: cbeav
   [Info] Expected, not present: cdparks
   [Info] Expected, not present: dukerutledge
   [Info] Expected, not present: halogenandtoast
   [Info] Checking package: yesod-page-cursor
   [Info] Expected, not present: halogenandtoast
   [Info] Expected, not present: mjgpy3
   ```

   See `--help` for more options.

1. If satisfied, re-run with `--fix` to actually adjust the maintainers lists

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
