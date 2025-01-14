#!/bin/bash -ex

# Take hostname as an argument
# If no argument is provided, raise an error
if [ -z "$1" ]; then
  echo "Usage: $0 <hostname>"
  exit 1
fi

# Function to set the hostname on Linux
set_linux_hostname() {
  new_hostname="$1"

  # Update the /etc/hostname file
  echo "$new_hostname" | sudo tee /etc/hostname >/dev/null

  # Update the current session's hostname
  sudo hostnamectl set-hostname "$new_hostname"

  # Update /etc/hosts to reflect the new hostname
  sudo sed -i "s/127.0.1.1.*/127.0.1.1 $new_hostname/" /etc/hosts ||
    echo "127.0.1.1 $new_hostname" | sudo tee -a /etc/hosts >/dev/null

  echo "Hostname updated to $new_hostname"
}

# Per-platform settings
case $(uname) in
Darwin)
  # Commands for macOS
  echo "Setting hostname on macOS..."
  sudo scutil --set HostName "$1"
  sudo scutil --set LocalHostName "$1"
  sudo scutil --set ComputerName "$1"
  echo "Hostname updated to $1 on macOS"
  ;;

Linux)
  # Commands for Linux (Debian-based or Alpine)
  echo "Setting hostname on Linux..."

  if grep -q "debian" /etc/os-release; then
    echo "Debian-based system detected"
    set_linux_hostname "$1"

  elif grep -q "alpine" /etc/os-release; then
    echo "Alpine Linux system detected"
    set_linux_hostname "$1"

  else
    echo "Unsupported Linux distribution"
    exit 1
  fi
  ;;

*)
  echo "Unsupported operating system: $(uname)"
  exit 1
  ;;
esac
