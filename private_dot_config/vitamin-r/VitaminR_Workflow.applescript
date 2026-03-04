-- Track whether the previous slice just elapsed (extension detection)
property sliceJustElapsed : false

-- Read current objective directly from Vitamin R plist
on readObjective()
    try
        set obj to do shell script "python3 ~/.config/vitamin-r/extract_objective.py"
        return obj
    on error
        return ""
    end try
end readObjective

-- Read session duration from Vitamin R plist with retries
on readMinutes()
    repeat 3 times
        try
            set secs to do shell script "defaults read net.publicspace.dist.vitaminr4 durationInSeconds"
            set m to (secs as integer) div 60
            if m > 0 then return m as string
        end try
        delay 0.4
    end repeat
    return "25"
end readMinutes

-- Helper: parse "OBJECTIVE:xxx|MINUTES:yyy" from spoken_message (fallback)
on parseObjective(msg)
    set obj to ""
    set startTag to "OBJECTIVE:"
    set startPos to (offset of startTag in msg)
    if startPos > 0 then
        set substr to (characters (startPos + (length of startTag)) thru -1 of msg) as string
        set pipePos to offset of "|" in substr
        if pipePos > 0 then
            set obj to text 1 thru (pipePos - 1) of substr
        else
            set obj to substr
        end if
    end if
    return obj
end parseObjective

-- Map objective name → HeyFocus profile name
on focusProfile(obj)
    if obj contains "Monthly Report" or obj contains "Administration" then
        return "admin"
    else if obj contains "Call" or obj contains "Meeting" then
        return "call"
    end if
    return "deep"
end focusProfile

-- Work session started (also called for 2/5 min extensions)
on time_slice_start(spoken_message)
    set isExtension to sliceJustElapsed
    set sliceJustElapsed to false

    set obj to my readObjective()
    if obj is "" then set obj to my parseObjective(spoken_message)
    set profile to my focusProfile(obj)

    -- For extensions plist may still hold the original session duration.
    -- Use a short fixed focus block so HeyFocus mirrors the brief extension.
    if isExtension then
        set mins to "5"
    else
        set mins to my readMinutes()
    end if

    do shell script "/run/current-system/sw/bin/aerospace workspace 4"

    if profile is not "" then
        do shell script "open 'focus://focus?minutes=" & mins & "&profile=" & profile & "'"
    else
        do shell script "open 'focus://focus?minutes=" & mins & "'"
    end if

    say obj
end time_slice_start

-- Work session finished
on time_slice_elapsed(spoken_message)
    set sliceJustElapsed to true
    say "Session done"
    do shell script "open 'focus://unfocus'"
end time_slice_elapsed

-- Work session stopped early
on time_slice_was_stopped(spoken_message)
    set sliceJustElapsed to false
    do shell script "open 'focus://unfocus'"
end time_slice_was_stopped

-- Progress reminder (silent)
on time_slice_in_progress(spoken_message, secondsLeft)
end time_slice_in_progress

-- Break started
on timed_break_start(spoken_message)
    say "Break time"
    do shell script "open 'focus://break'"
end timed_break_start

-- Break reminder (silent)
on timed_break_reminder(spoken_message, secondsLeft)
end timed_break_reminder

-- Break finished → resume with workspace switch
on timed_break_end(spoken_message)
    say "Break done, back to work"
    do shell script "/run/current-system/sw/bin/aerospace workspace 4"
end timed_break_end
