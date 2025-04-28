
# Game is ready.

[MineSweeperD12.exe](https://github.com/AlexandrNevskiy/MineSweeperD12/blob/main/MineSweeperD12.exe)

---
TODO:

1. ●●●●● Visually close to classic (new classic) Minesweeper
2. ●●●●● Grid, Mines, Flags, Left-Right click
3. ●●●●● Dificulty settings to Beginner, Intermediate, Advance and Custom grid size
4. ●●●●● High Score table 
5. ●●●●● Personal statistics
6. ●●●●● Save game at exit
7. ●●●●● Restore game at start
8. ●●●●● Alghorithm to prevent First-Click-Loose
9. ●●●●● Save game window position at exit and restore at start
10. ●●●●● Sounds 
11. ●●●●● Sound configs
12. ●●●●● Save/Restore settings
13. ●●●●● Loose condition
14. ●●●●● Win Condition
15. ●●●●◌ Final polishing and testing

👇
<details><summary>Behind the scenes</summary>

Gradient

A set of 30 images of varying brightness is used to display tiles. When the playing field is initialized, a Gradient Field is generated - it matches the size of the playing field and in each cell contains tile indices that will be used to draw the main field.
To build a rectangular gradient:
1. The sum of the length and width of the playing field is taken - we get a one-dimensional size onto which a set of 30 tiles is superimposed. To keep the playing field as dark as possible, if this size is less than 30, then it is used to index the tiles, and starts with the darkest one.
2. Having interpolated data, you can get gradient values ​​for the upper right and lower left corners.
3. The rest of the field is filled with sequential interpolation of each column.

![image](https://github.com/user-attachments/assets/b6bbf9e7-e1f4-4a06-88eb-b8c63026162f)

---

Safe first click

Initially, a draft field is created, the same size as the main playing field. Its center is found and, depending on the specified dimensions, a zone is created that is prohibited for generating mines. (for example, 1/5 of the field size, but not less than 3x3)
After this, the coordinates of all other cells are entered into the list.
Next, a random index is selected in the list. According to this index, coordinates are taken from the list and a mine is placed in them. The entry just selected is removed from the list.
The required number of times (depending on the total number of mines required for generation) is repeated in a cycle. This is guaranteed to create the required number of mines in the guaranteed time.
After this, the game goes into the first click wait mode.

![image](https://github.com/user-attachments/assets/db78c4f3-ae56-411a-baae-7ac3aeaedeee)

Let's say (for ease of explanation), the player clicked on a tile that is highlighted in color.

![image](https://github.com/user-attachments/assets/fbe02413-31b2-4901-9ce4-e3201e61881f)

For this place, the horizontal and vertical offset value is calculated and the entire draft field is rewritten into the playing field with this offset, placing a safe zone under the player's click.

![image](https://github.com/user-attachments/assets/54a06b1a-37ee-49dc-8705-9f129ced313b)

</details> 
☝️


---
2025-04-26 2<br/>

About screen:

![437768625-12deaf17-47af-49f1-b172-7088f0bdb6c3](https://github.com/user-attachments/assets/c3c16795-4c93-4c87-87dc-c46bf72f7b46)


---
2025-04-26<br/>

Personal statistics and high score. Separated for difficulty (except Custom). For each user.
(As Date format used locale short date format)

![437669643-be0a59ce-5563-48a7-a81a-1f2b720fc07b](https://github.com/user-attachments/assets/208534a6-180d-4d29-827b-b112e709bbc6)

![437669709-7aa53cbe-b5e4-42a7-88f2-02b75227658a](https://github.com/user-attachments/assets/46df9901-bc07-4699-b5d4-4110ae3f631f)

![437669871-c1b16a05-3cd0-441f-ace9-52cd86cc0d7f](https://github.com/user-attachments/assets/71d7618f-47da-4d9d-a214-9c5d1c0a9a2b)

---
2025-04-25<br/>

Game is fully functional (highscore - WIP)

Redesigned Options:<br/>
![437282842-e3fc55f6-0b20-4989-9612-b2e4c1a6a013](https://github.com/user-attachments/assets/5f6e4933-5404-4ec9-aa44-4adcc7e8ad14)

Losse state:<br/>
![437134424-737ca975-1db3-4075-95d1-02a90ad0320c](https://github.com/user-attachments/assets/667f977f-c076-437c-b43f-4bd608facf6f)

Win state:<br/>
![437274994-11c1d75a-8e98-4f18-8962-997965865c04](https://github.com/user-attachments/assets/bd39d148-7cd3-4e3f-80e9-d2bd89849375) 

Win state without flags:<br/>
![437276020-d102529f-4d76-4a3f-8357-ce0cf4068b5b](https://github.com/user-attachments/assets/3c726ed4-a1b7-4bdd-b8a9-f8a77685d9fb)

---
2025-04-23

10. ●●●●● Sounds 
11. ●●●●● Sound configs
12. ●●●●● Save/Restore settings
13. ●◌◌◌◌ Loose condition
14. ●◌◌◌◌ Win Condition

Sounds, sound config, Left-Right-Click check, Left-Right click detector based on WinAPI

![image](https://github.com/user-attachments/assets/e0c87cc7-3897-41ac-8eba-506cfa85862b)

---
2025-04-19

Heavily refactored code.

Tile shadows, timer, mines-flags count, safe-start, correct open empty space

![434607331-fa245c9b-36f9-4398-9bc1-eff0ab690e64](https://github.com/user-attachments/assets/a0eaaab8-3a0d-47d8-874b-9759554f7d50)



---
2025-04-10

Mine field generation, gradient grid, mines

![432637222-4d5af787-30b1-4003-9e08-512cd4fe5392](https://github.com/user-attachments/assets/3e0d726b-5b93-4090-919a-0123a9e0c00d)

![image](https://github.com/user-attachments/assets/01a0a8d2-98fe-46dc-a055-62963febc5f0)



---
2025-04-09

First approach, Dificulty settings

![image](https://github.com/user-attachments/assets/a5238e67-f7ca-4c59-a4a0-71afab9c47d3)
