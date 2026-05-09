# vhs tape for docs/demo.gif
# render with: vhs docs/demo.tape
# requires the local color-enabled build at _build/default/bin/main.exe

Output docs/demo.gif

Set Shell "bash"
Set Theme "TokyoNight"
Set FontSize 20
Set Width 1400
Set Height 820
Set Padding 24
Set TypingSpeed 35ms
Set PlaybackSpeed 1.0

Hide
Type "cd /Users/pinetortoise/Desktop/morph/examples/demo"
Enter
Type "alias morph=/Users/pinetortoise/Desktop/morph/_build/default/bin/main.exe"
Enter
Type "export MORPH_COLOR=always"
Enter
Type `export PS1='\[\e[34m\]~/morph/examples/demo\[\e[0m\] \[\e[32m\]$\[\e[0m\] '`
Enter
Type "clear"
Enter
Sleep 400ms
Show

Sleep 700ms
Type "morph rewrite -p 'console.log($$$ARGS)' -r 'logger.info($$$ARGS)' app.ts"
Sleep 500ms
Enter
Sleep 3500ms

Type "morph rewrite -p 'console.log($X)' -w '$X: number' app.ts"
Sleep 500ms
Enter
Sleep 4500ms
