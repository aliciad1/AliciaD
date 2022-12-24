#Alicia Doung
#Homework 3
#Description: Holds the main menu display and the function for menu inputs.


import difficulty as d
import new_game as g

#The main menu display for MEMOGAME
def main_menu(top_score):
    print("=========== MEMOGAME ==========")
    print("|                             |")
    print("|1.Choose level of difficulty |")
    print("|2.Start Game                 |")
    print("|3.Exit the Game              |")
    print("|                             |")
    print("|Current Difficulty:", level, "/5     |")
    print("|Highest Score Reached:", (g.get_high_score()), "    |")
    print("-------------------------------")
    options()

#Allows the user to interact with the main menu display and choose an option
def options():
    user_choice = input("Enter an option: ")

    if user_choice == "1":
        global level
        level = d.difficulty_level()
        global high_score
        main_menu(high_score)
    elif user_choice == "2":
        g.game(level)
        
    elif user_choice == "3":
        print("Thanks for playing MEMOGAME")
        quit
    else:
        print("Invalid option, try again: ")
        options()
        
    
level = "1"
high_score = "0"
if __name__ == '__main__':
    main_menu(high_score)
 
