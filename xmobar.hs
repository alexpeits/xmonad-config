Config {
    font = "xft:Iosevka Term SS02:size=11.5",
    bgColor = "#252525",
    fgColor = "#95c2be",
    border = FullB,
    borderWidth = 2,
    borderColor = "#252525",
    position = TopSize L 94 25,
    commands = [
        Run Cpu [
          "-t", "cpu <total>",
          "-L", "10",
          "-H", "60",
          "--normal", "#2bc9a2",
          "--high", "#e44439",
          "-S", "True"
        ] 5,
        Run Memory [
          "-t", "mem <usedratio>",
          "-L", "10",
          "-H", "60",
          "--normal", "#2bc9a2",
          "--high", "#e44439",
          "-S", "true"
        ] 5,
        Run Date "%a %b %d %H:%M" "date" 5,
        Run Kbd [("us", "us"), ("gr", "gr")],
        Run Volume "default" "Master" [
            "-t", "vol <volume> <status>",
            "-S", "True",
            "--",
            "-O", "",
            "-o", "[x] ",
            "-c", "#e44439"
        ] 5,
        Run Battery [
            "-t", "bat <left> <acstatus>",
            "-L", "15",
            "-H", "70",
            "--low", "#dc7747",
            "--normal", "#2894b5",
            "--high", "#2bc9a2",
            "--",
            "-f", "ACAD/online",
            "-O", "ch",
            "-o", "bt",
            "-i", "ac",
            "-P"
        ] 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %cpu% | %memory% | %kbd% | %default:Master%| %battery% | <action=`gsimplecal` button=1><fc=#53bcd6>%date%</fc></action> ",
    lowerOnStart = True
}
