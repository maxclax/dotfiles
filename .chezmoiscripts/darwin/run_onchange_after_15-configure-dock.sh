#!/bin/bash

set -eufo pipefail

trap 'killall Dock' EXIT

declare -a remove_labels=(
  Launchpad
  Messages
  Mail
  Maps
  Photos
  FaceTime
  Calendar
  Contacts
  Reminders
  Notes
  Freeform
  TV
  Music
  Keynote
  Numbers
  Pages
  "App Store"
  "System Settings"
  "iPhone Mirroring"
  "Send to Kindle"
)

for label in "${remove_labels[@]}"; do
  dockutil --no-restart --remove "${label}" || true
done

# Add applications to the dock
declare -a add_apps=(
  "/Applications/DEVONthink 3.app"
  "/Applications/Fantastical.app"
  "/Applications/OmniFocus.app"
  "/Applications/Logseq.app"
  "/Applications/Drafts.app"
  "/Applications/Airmail.app"
)

for app in "${add_apps[@]}"; do
  dockutil --no-restart --add "${app}" || true
done

# Add a directory with applications near the trash
dockutil --no-restart --add '/Applications' --view grid --display folder --sort name --position end || true
