#Alicia Doung
#Homework 3
#Description: Calculates points earned based on game performance.

#Calculates and returns the points the user has earned based on the level of difficulty played and performance. 
def score(diff, correct):
    points = int(diff) * correct * 10
    return points

