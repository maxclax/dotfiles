-- Timestamp (seconds since epoch) when the last slice elapsed; 0 = none
property sliceElapsedAt : 0

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

-- Parse "MINUTES:yyy" from spoken_message — gives the actual slice duration
-- even for continuations where the plist still holds the original duration.
on parseMinutesFromMsg(msg)
    set startTag to "MINUTES:"
    set startPos to (offset of startTag in msg)
    if startPos > 0 then
        set substr to (characters (startPos + (length of startTag)) thru -1 of msg) as string
        set pipePos to offset of "|" in substr
        if pipePos > 0 then
            set m to text 1 thru (pipePos - 1) of substr
        else
            set m to substr
        end if
        try
            if (m as integer) > 0 then return m
        end try
    end if
    return ""
end parseMinutesFromMsg

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
    set now to (do shell script "date +%s") as integer
    -- 300 s window: covers the case where the user takes up to ~5 min to click Continue
    set isExtension to (sliceElapsedAt > 0 and (now - sliceElapsedAt) ≤ 300)
    set sliceElapsedAt to 0

    set obj to my readObjective()
    if obj is "" then set obj to my parseObjective(spoken_message)
    set profile to my focusProfile(obj)

    -- Prefer the duration embedded in spoken_message (accurate for continuations).
    -- Fall back to isExtension heuristic (plist may still hold the original duration).
    set mins to my parseMinutesFromMsg(spoken_message)
    if mins is "" then
        if isExtension then
            set mins to "5"
        else
            set mins to my readMinutes()
        end if
    end if

    do shell script "/run/current-system/sw/bin/aerospace workspace 4"

    if profile is not "" then
        do shell script "open 'focus://focus?minutes=" & mins & "&profile=" & profile & "'"
    else
        do shell script "open 'focus://focus?minutes=" & mins & "'"
    end if

end time_slice_start

-- Work session finished
on time_slice_elapsed(spoken_message)
    set sliceElapsedAt to (do shell script "date +%s") as integer
    do shell script "open 'focus://unfocus'"
end time_slice_elapsed

-- Work session stopped early
on time_slice_was_stopped(spoken_message)
    set sliceElapsedAt to 0
    do shell script "open 'focus://unfocus'"
end time_slice_was_stopped

-- Progress reminder (silent)
on time_slice_in_progress(spoken_message, secondsLeft)
end time_slice_in_progress

-- Break started
on timed_break_start(spoken_message)
    do shell script "open 'focus://break'"
end timed_break_start

-- Break reminder (silent)
on timed_break_reminder(spoken_message, secondsLeft)
end timed_break_reminder

-- Break finished → resume with workspace switch
on timed_break_end(spoken_message)
    do shell script "/run/current-system/sw/bin/aerospace workspace 4"
end timed_break_end
