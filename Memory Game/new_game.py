#Alicia Doung
#Homework 3
#Description: Holds the main game function as well as associated functions that do simpler tasks for the game to properly run.

import random as r
import time as t
import difficulty as d
import score as s

high_score = 0

#The function that will have the game wait 10 seconds before printing blank lines to make the given numbers "disappear". It also tells us the program is waiting for 10 seconds and when it is over.
def wait():
    print("The round starts in 10 seconds!")
    t.sleep(10)
    print("10 seconds passed!")
    print("\n" * 50)

#Allows the high score to be turned into, and returned as, a string
def get_high_score():
    global high_score
    return str(high_score)

#Provides the menu options for when the player wins a round
def won_options(d, points):
    entry = input("Enter 1 to play another round, 2 to see the main menu, or 3 to exit: ")
    if entry == '1':
        game(d)
    elif entry == '2':
        import menu as m
        m.main_menu(points)
    elif entry == '3':
        quit
        print('Thanks for playing MEMOGAME')
    else:
        print("Invalid input.")
        over_options(d, points)

#Provides the information for when the player loses a round       
def lost_options(d, points):
    entry = input("Enter 1 to see the main menu, or 2 to exit: ")
    if entry == '1':
        import menu as m
        m.main_menu(points)
    elif entry == '2':
        quit
        print('Thanks for playing MEMOGAME')
    else:
        print("Invalid input.")
        lost_options(d, points)

#Evaluates if the user's guessed value is correct, returning it as true       
def is_correct(correct_val, guess):
    if correct_val == guess:
        return True
    else:
        return False

#Will return back how many numbers the game must display for memorization (based on the chosen level difficulty).
def total_numbs(n):
    if n == '1':
        numbers = 3
    elif n == '2':
        numbers = 4
    elif n == '3':
        numbers = 5
    elif n == '4':
        numbers = 6
    else:
        numbers = 7
    return numbers

#The main game function itself that will provide the list of numbers to memorize, evaluate if the memorized numbers are correct, and then produce a score based on the correctly memorized numbers.
def game(d):
    global high_score
    games = True
    correct_vals = 0

    while games:

        print("-Level", d, "-")

        #sets how many numbers are shown per level
        numbers = total_numbs(d)
        
        #list of numbers
        memorize = []

        for i in range(0, numbers):
            #set the range of possible given numbers based on the level
            if '1' <= d <= '2':
                w = str(r.randint(1,99))
            elif '3' <= d <= '4':
                w = str(r.randint(10,9999))
            else:
                w = str(r.randint(100,999999))
            memorize.append(w)
            
        for x in memorize:
            print(x)
        
        wait()

        i=0          
        while i < len(memorize):
            if i == 0:
                guess = input("Enter in the first number: ")
            else:
                guess = input("Enter in the next number: ")
            if is_correct(memorize[i],guess): #first attempt is correct
                i += 1
                correct_vals += 1
                continue
            else: #second attempt
                second_guess = input("Incorrect, try again: ")
                if is_correct(memorize[i], second_guess):
                    i += 1
                    correct_vals += 1
                    continue
                else: #wrong second attempt
                    points = s.score(d, correct_vals)
                    print("Game over! Your score: ", points)
                    if points > high_score:
                        high_score = points
                    lost_options(d, points)
                    break
                    
                    
        points = s.score(d, correct_vals)            
        if correct_vals == len(memorize):
                print("Congratulations. You won the round! Your score: ", points)
                if points > high_score:
                    high_score = points
                won_options(d, points)
                
        
        break
        
        games = False

        


