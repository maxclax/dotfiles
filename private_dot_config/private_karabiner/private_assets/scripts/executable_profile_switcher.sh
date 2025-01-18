#!/bin/zsh

# Ensure PATH includes the directory where karabiner_cli is located
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"

get_karabiner_profile(){
# Get the current profile using karabiner_cli
  local retcode response
  response="$(karabiner_cli --show-current-profile-name 2>&1)"
  retcode=$?
  if [ "$retcode" != 0 ]; then
    osascript -e 'display notification "'"$0: Error running karabiner_cli: '$response'. retcode: $retcode."'" with title "Karabiner Elements Profile Switcher"'  
    exit "$retcode"
  fi

  echo "$response"
}

# Check if karabiner_cli is in the expected directory
if [ ! -f "/opt/homebrew/bin/karabiner_cli" ]; then
  echo "karabiner_cli not found in /opt/homebrew/bin"
fi

if ! $(which karabiner_cli &> /dev/null);then 
  osascript -e 'display notification "'"$0: Can't find karabiner_cli. Add it to your path variable."'" with title "Karabiner Elements Profile Switcher"' 
  exit 1
fi

profiles=()
# get profile names via karabiner_cli
while IFS= read -r line; do
  profiles+=("$line")
done < <(karabiner_cli --list-profile-names)


current_profile="$(get_karabiner_profile)"

# Check if the current profile is recognized in the list
profile_index="$(($profiles[(Ie)$current_profile]))"


if [[ "$profile_index" = 0 ]]; then
  osascript -e 'display notification "'"$0: Current profile: $current_profile not found in profiles list."'" with title "Karabiner Elements Profile Switcher"'
  exit 2
fi


# Calculate the next profile index
next_index=$(( (profile_index % ${#profiles}) + 1 ))
#echo "next index: $next_index"

# Set the next profile
next_profile="${profiles[$next_index]}"

# Change the Karabiner profile
karabiner_cli --select-profile "$next_profile"



#make sure karabiner-elements is running in the background
if ! $(ps aux | grep Karabiner-Elements | grep -qEv "karabiner_observer|karabiner_session_monitor|karabiner_grabber|grep"); then
  osascript -e 'display notification "'"Starting Karabiner-Elements.."'" with title "Karabiner Elements Profile Switcher"'
  open -a Karabiner-Elements
  retcode=$?
  if [ "$retcode" -ne 0 ]; then
    osascript -e 'display notification "Karabiner-Elements failed to start. \"open -a Karabiner-Elements\" failed with exit code '"$retcode"'." with title "Karabiner Elements Profile Switcher"'
  fi
fi

# Display a notification
current_profile="$(get_karabiner_profile)"

if [ "$current_profile" = Karabiner-Elements-Activated ]; then
  osascript -e 'display notification "'"Karabiner-Elements is now Activated."'" with title "Karabiner Elements Profile Switcher"'
elif [ "$current_profile" = Karabiner-Elements-Deactivated ]; then
  osascript -e 'display notification "'"Karabiner-Elements is now Deactivated."'" with title "Karabiner Elements Profile Switcher"'
else
  osascript -e 'display notification "'"Karabiner-Elements profile: $(karabiner_cli --show-current-profile-name 2>/dev/null) is now active."'" with title "Karabiner Elements Profile Switcher"'
fi
