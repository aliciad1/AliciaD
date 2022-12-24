#
#
#

def difficulty_level():
    
    level_choice = input("Choose a difficulty level: ")
        
    if level_choice == "1":
        difficulty = "1"
    elif level_choice == "2":
        difficulty = "2"
    elif level_choice == "3":
        difficulty = "3"
    elif level_choice == "4":
        difficulty = "4"
    elif level_choice == "5":
        difficulty = "5"
    else:
        print("Choose an appropriate difficulty")
        difficulty = difficulty_level()
       
    return difficulty



