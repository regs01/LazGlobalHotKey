$InkscapeExecutable = "c:/Program Files/Inkscape/bin/inkscape.com"
#$Sizes = 16, 20, 24, 28, 32, 36, 40, 44, 48, 56, 64, 96, 128, 192, 256, 384, 512, 1024
#$Sizes = 16, 20, 24, 28, 32, 36, 40, 44, 48, 56 # tray icon sizes only
#$Sizes = 24, 48 # tray icon sizes only
$Sizes = 36 # tray icon sizes only
$OriginalFile = "lazglobalhotkey_225.svg"
$FileName = (Get-Item "$OriginalFile").Basename
$ExportedFileTemplate = "{0}{1}.png"

if (!(Test-Path $InkscapeExecutable)) {
  Write-Output "Inkscape is not found."
  Read-Host
  Exit
}

foreach ($size in $Sizes) {
  Write-Host "Exporting size: $size"
  $ExportedFile = $ExportedFileTemplate -f $FileName, $size
  Start-Process $InkscapeExecutable -ArgumentList "--export-filename $ExportedFile --export-area-page --export-width $size --export-height $size $OriginalFile" -Wait -NoNewWindow
}

Read-Host "Press Enter to close."
