$Logtalk = $Env:LOGTALKHOME
$Env:LOGTALK_STARTUP_DIRECTORY = "C:\Dev\workspace\ECTrafficModeller\"
$Config = $Env:LOGTALK_STARTUP_DIRECTORY
$App = $Logtalk + "\integration\logtalk_swi.pl"
write-output "Pfad to execute" , $App, $Config
C:\Dev\swipl\bin\swipl-win.exe -s  $App $Config\logic\loader.lgt