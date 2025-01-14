#!/bin/bash

# Exit immediately if a command exits with a non-zero status, treat unset variables as an error,
# disable filename expansion (globbing), and return the exit status of the last command in the pipeline that failed.
set -eufo pipefail

# Disable swipe navigation with two-finger scrolling (e.g., back/forward gestures in browsers)
defaults write -g AppleEnableSwipeNavigateWithScrolls -int 0

# Disable minimizing windows when double-clicking the window title bar
defaults write -g AppleMiniaturizeOnDoubleClick -int 0

# Disable the press-and-hold feature for keys (enables key repeat instead)
defaults write -g ApplePressAndHoldEnabled -int 0

# Show all file extensions in Finder
defaults write -g AppleShowAllExtensions -int 1

# Enable cursor magnification when locating the cursor with a shake
defaults write -g CGDisableCursorLocationMagnification -int 0

# Set the initial key repeat delay (in milliseconds). Lower value means faster response
defaults write -g InitialKeyRepeat -int 15

# Set the key repeat rate (the speed at which a key repeats when held down). Lower value means faster repeat
defaults write -g KeyRepeat -int 2

# Disable automatic capitalization
defaults write -g NSAutomaticCapitalizationEnabled -int 0

# Disable automatic substitution of double dashes (--) with an em dash (—)
defaults write -g NSAutomaticDashSubstitutionEnabled -int 0

# Disable automatic inline text predictions
defaults write -g NSAutomaticInlinePredictionEnabled -int 0

# Disable automatic substitution of double spaces with a period
defaults write -g NSAutomaticPeriodSubstitutionEnabled -int 0

# Disable automatic substitution of straight quotes with smart quotes
defaults write -g NSAutomaticQuoteSubstitutionEnabled -int 0

# Disable automatic spelling correction
defaults write -g NSAutomaticSpellingCorrectionEnabled -int 0

# Disable all forms of automatic text correction
defaults write -g NSAutomaticTextCorrectionEnabled -int 0

# Save new documents to the local disk by default (instead of iCloud)
defaults write -g NSDocumentSaveNewDocumentsToCloud -int 0

# Clear custom text replacements (like auto-correct shortcuts)
defaults write -g NSUserDictionaryReplacementItems '()'

# Disable automatic spelling correction in web views (e.g., Safari)
defaults write -g WebAutomaticSpellingCorrectionEnabled -int 0

# Enable function keys (F1, F2, etc.) to behave as standard function keys
# (requires holding the Fn key to use special features like brightness or volume)
defaults write -g com.apple.keyboard.fnState -int 1

# Disable natural scrolling direction (reverts scrolling to traditional behavior)
defaults write -g com.apple.swipescrolldirection -int 0

# Disable force click on the trackpad
defaults write -g com.apple.trackpad.forceClick -int 0

# Automatically hide the Dock when not in use
defaults write com.apple.dock autohide -int 1

# Position the Dock on the bottom|left|right side of the screen
defaults write com.apple.dock orientation -string left

# Disable showing recent applications in the Dock
defaults write com.apple.dock show-recents -int 0

# Disable the gesture for showing the desktop (usually a spread with three or four fingers)
# defaults write com.apple.dock showDesktopGestureEnabled -int 0

# Disable the gesture for showing Launchpad (usually a pinch with four fingers)
defaults write com.apple.dock showLaunchpadGestureEnabled -int 0

# Disable the gesture for showing Mission Control (usually a swipe up with three or four fingers)
defaults write com.apple.dock showMissionControlGestureEnabled -int 0

# Show the full POSIX path in the Finder window title (e.g., /Users/username/Documents)
defaults write com.apple.finder _FXShowPosixPathInTitle -int 1

# Set the default Finder view to list view ("Nlsv" stands for "List View")
defaults write com.apple.finder FXPreferredViewStyle -string Nlsv

# Sort folders first when viewing files in Finder
defaults write com.apple.finder _FXSortFoldersFirst -int 1

# Automatically remove items from the Trash after 30 days
defaults write com.apple.finder FXRemoveOldTrashItems -int 1

# Disable the warning when changing a file's extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -int 0

# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Don't create .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Enable move windows at any part of the window
defaults write -g NSWindowShouldDragOnGesture -bool true
