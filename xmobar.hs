-- xmobar config

Config {
    font = "xft:DejaVu Sans Mono:size=11:antialias=true",
    borderColor = "black",
    border = TopB,
    bgColor = "#0c1014",
    fgColor = "#a4d6d2",
    position = Static { xpos = 0, ypos = 0, width = 3644, height = 28 },
    commands = [
        -- Run Network "enp0s31f6" ["-L","0","-H","32","--normal","#2bc9a2","--high","#e44439"] 30,
        -- Run Network "wlp3s0" ["-L","0","-H","32","--normal","#2bc9a2","--high","#e44439"] 30,
        Run Cpu ["-t", "cpu: <total>%", "-L","3","-H","50","--normal","#2bc9a2","--high","#e44439"] 30,
        Run Memory ["-t","mem: <usedratio>%"] 30,
        Run Date "%a %b %d %H:%M" "date" 10,
        Run Com "/bin/bash" ["-c", "~/bin/get-volume.sh"] "myvolume" 2,
        Run Com "/bin/bash" ["-c", "~/bin/getlayout.sh"] "mylayout" 2,
        Run Com "/bin/bash" ["-c", "~/bin/song-info.sh"] "mymusic" 10,
        Run Brightness ["--template", "☀ <percent>%", "--", "-D", "intel_backlight"] 5,
        Run Battery [
            "-t", "<acstatus> <left>%",
            "--Low", "20",
            "--High", "80",
            "--low", "#e44439",
            "--normal", "#dc7747",
            "--high", "#2bc9a2",
            "--",
            --"-c", "charge_full",
            "-f", "ACAD/online",
            "-O", "⚡",
            "-o", "▅",
            "-h", "#2bc9a2",
            "-l", "#e44439"
        ] 20,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader%}{ %cpu% | %memory% | ♫ %myvolume%<fc=#64807f>%mymusic%</fc> | %mylayout% | <action=`gsimplecal` button=1><fc=#53bcd6>%date%</fc></action> ",
    lowerOnStart = True
}
