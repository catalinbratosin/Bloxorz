# Bloxorz
Implementation of the [_Bloxorz_](https://www.miniclip.com/games/bloxorz/ro/) game alongside a self solving mechanism of the game using DFS technique.

## How to play
The game involves shuffling a piece of block across a maze. The objective is basically to get the block to fall through a square hole at the end of each stage, all the while trying to avoid falling off the edges.

## Description
The game has 2 main elements:
- `Block`: the parallelepiped that can be rolled in one of the 4 directions(North, South, East, West). It also has an orientation that can be Vertical, Horizontal or Standing.
    * block = '▓'
- `Cell`: the tiles that form the maze. There are different types of cells:
    * hardTile = '▒' -> sustain the block in any orientation
    * softTile = '=' -> can't sustain the block if it has a standing orientation
    * switch = '±' -> special tiles which sustain the block in any orientation and also can add/remove a numeber of hard tiles when you step on them
    * emptySpace = '-' -> the block falls 
    * winningTile = '*' -> you have to reach this unique tile and also to be on a standing position to win the current level 

## Usage
In order to play the game in command line you have to type  
 - `stack exec ghci Interactive.hs`  
 
After loading packages and compiling sources you can play the game by typing
 - `play level<number>` , choose a number between 0 and 6 instead of `<number>` field  
 
Or you can let the AI play it for you if you type
 - `visualize level<number> False` choose a number as above
 
 When you play the game you have to press __w,a,s,d__ followed by __ENTER__ to move the block in the desire direction. Try to reach `*` and stay away from `-`. :warning:

  
### Examples Level 1
|Start|Win|Lose|
|:---:|:---:|:---:|
|<img src="https://i.imgur.com/Ak21Nxu.png"/>|<img src="https://i.imgur.com/aCpvurj.png"/>|<img src="https://i.imgur.com/8tT5OjC.png"/>|
