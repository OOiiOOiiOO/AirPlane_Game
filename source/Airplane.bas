  rem **********************************
 rem *<Tinkernut World>               *
 rem *<Shoot the squirrel before he  *
 rem *eats you.>                      *
 rem *<admin@tinkernut.com>           *
 rem *<www.tinkernut.com>             *
 rem **********************************


 rem Create the title screen
opening
 playfield:
 X...X.XXXX.XXXXX.X....XXX.X..X
 XX.XX.X..X.X...X.X.....X..X.X.........
 X.X.X.XXXX.XXXXX.X.....X..XX....
 X...X.X..X.X..X..X.....X..X.X...........
 X...X.X..X.X...X.XXXX.XXX.X..X...
 .................................................
 ......X...X.X...X.XXX.......................
 ......X...X.XX..X..X.........................
 ......X...X.X.X.X..X.........................
 ......X...X.X..XX..X.........................
 .......XXX..X...X.XXX.....................
 .................................................
end


 rem Loop the screen until the spacebar is pressed
title
 COLUBK = $9E
 COLUPF = 00
 drawscreen
 if joy0fire || joy1fire then goto skiptitle
 goto title

 rem This function displays after the title is skipped
skiptitle

 rem Colors
 COLUPF = 00
 COLUBK =  $9E

 rem Player location
 player0x = 50 : player0y = 50
 player1x = 20 : player1y = 20

 rem Score setting and color
 score = 5 : scorecolor = 0

 rem Missle size and location
 missile0height=4:missile0y=255
 NUSIZ0 = 16

 rem Create a variable to keep up with lives
 a = 5

 rem Create the playfield
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 ...............................
 ...............................
 ...............................
 ...............................
 ...............................
 ...............................
 ...............................
 ...............................
 ...............................
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

 rem This main function is what loops constantly
main

 rem This is the animation function
animate
 rem This frame variable slows down the animation
 v = v + 1 
 
 rem This code animates the sprites
 if v = 7 && w = 0 then ax
 if v = 7 && w = 1 then bx
 if v = 7 && w = 2 then cx
 if v = 7 && w = 3 then dx

 goto nextstep
 
 rem These four sprites are different stages of the animation
ax
 v = 0
 w = 1
 player1:
 %00011000
 %10111101
 %01111110
 %00111100
 %00111100
 %00100100
 %01000010
 %10000001
end

 goto nextstep
 
bx
 v = 0
 w = 2
 player1:
 %00011000
 %10111101
 %01111110
 %00111100
 %00111100
 %00100100
 %01000010
 %10000001
end

 goto nextstep

cx
 v = 0
 w = 3
 player1:
 %00011000
 %10111101
 %01111110
 %00111100
 %00111100
 %00100100
 %01000010
 %10000001
end

 goto nextstep
 
dx
 v = 0
 w = 0
 player1:
 %00011000
 %10111101
 %01111110
 %00111100
 %00111100
 %00100100
 %01000010
 %10000001
end

 goto nextstep

 rem Create acorn sprite
nextstep
 player0:
 %01000001
 %00101010
 %00011100
 %00111110
 %00001000
 %00101010
 %00011100
 %00001000
end
 rem check to see if a missile has already been fired
checkfire
 if missile0y>240 then goto skip
 missile0y = missile0y - 2 : goto draw

 rem if a missile hasn't been fired, then fire missile
skip
 if joy0fire then missile0y=player0y-2:missile0x=player0x+4

 rem Draw output to screen
draw
 drawscreen

 rem Fix player wraparound bug
 if player0x < 8 then player0x = 8
 if player0x > 150 then player0x = 150
 if player0y < 8 then player0y = 8
 if player0y > 84 then player0y = 84

 rem Have player 1 chase player 2
 if player1y < player0y then player1y = player1y + 1
 if player1y > player0y then player1y = player1y - 1
 if player1x < player0x then player1x = player1x + 1
 if player1x > player0x then player1x = player1x - 1
 player1x = player1x : player1y = player1y


 rem Detect missile collision with squirrel
 if collision(missile0,player1) then score=score+1:player1x=rand/2:player1y=0:missile0y=255:goto pointsound
 rem Detect squirrel collision with the acorn
 if collision(player0,player1) then score=score-1:player1x=rand/2:player1y=0:missile0y=255:a=a-1:goto deadsound


 rem joystick movements
 if joy0up then player0y = player0y-1 : goto skipmove
 if joy0down then player0y = player0y+1 : goto skipmove
 if joy0left then player0x = player0x-1 : goto skipmove
 if joy0right then player0x = player0x +1 : goto skipmove

 rem refresh the screen
skipmove
 goto main

 rem Play point sound
pointsound
 AUDV0 = 5 : AUDC0 = 4 : AUDF0 = 10
 p = p + 1
 drawscreen
 if p < 2 then pointsound
 p = 0
 AUDV0 = 0 : AUDC0 = 0 : AUDF0 = 0
 goto main

 rem Play dead sound
deadsound
 AUDV1 = 10
 AUDC1 = 7
 AUDF1 = 12
 p = p + 1
 drawscreen
 if p < 10 then deadsound
 p = 0
 AUDV1 = 0 : AUDC1 = 0 : AUDF1 = 0
 if a = 0 then goto opening
 goto main












