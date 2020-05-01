Config {
  font = "xft:Iosevka Term:size=11.5",
  bgColor = "#272727",
  fgColor = "#afc7c5",
  border = FullB,
  borderWidth = 2,
  borderColor = "#272727",
  position = TopSize L 95 25,
  commands = [
    Run StdinReader,
    Run Com "bash" ["-c", "~/bin/song-info.sh spotify,vlc"] "music" 10,
    Run Cpu [
      "-t", "cpu <total>",
      "-L", "10",
      "-H", "60",
      "--normal", "#A3BE8C",
      "--high", "#BF616A",
      "-S", "True",
      "-m", "3", "-c", " ", "-a", "l"
    ] 10,
    Run Memory [
      "-t", "mem <usedratio>",
      "-L", "10",
      "-H", "60",
      "--normal", "#A3BE8C",
      "--high", "#BF616A",
      "-S", "true",
      "-m", "3", "-c", " ", "-a", "l"
    ] 10,
    Run Volume "default" "Master" [
      "-t", "vol <volume><status>",
      "-S", "True",
      "-m", "3", "-c", " ", "-a", "l",
      "--",
      "-O", "",
      "-o", " x",
      "-c", "#BF616A"
    ] 5,
    Run Battery [
      "-t", "bat <left> <acstatus>",
      "-L", "20",
      "-H", "70",
      "--low", "#BF616A",
      "--normal", "#88C0D0",
      "--high", "#A3BE8C",
      "-m", "2", "-c", " ", "-a", "l",
      "--",
      "-f", "ACAD/online",
      "-O", "<fc=#D08770>ch</fc>",
      "-o", "bt",
      "-i", "<fc=#EBCB8B>ac</fc>",
      "-A", "10",
      "-a", "dunstify -a 'history-ignore' -r 2345 -u critical 'Low battery'",
      "-P"
    ] 10,
    Run Kbd [("us", "us"), ("gr", "gr")],
    Run Date "%a %b %d %H:%M" "date" 5
  ],
  alignSep = "}{",
  template = "%StdinReader% }{ <fc=#B48EAD>%music%</fc> <fc=#4f6b69>|</fc> %cpu% <fc=#4f6b69>|</fc> %memory% <fc=#4f6b69>|</fc> %default:Master% <fc=#4f6b69>|</fc> %battery% <fc=#4f6b69>|</fc> %kbd% <fc=#4f6b69>|</fc> <action=`gsimplecal` button=1><fc=#88C0D0>%date%</fc></action> ",
  lowerOnStart = True
}
