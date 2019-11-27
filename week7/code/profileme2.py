""" Similar script to profileme but the functions (which use to have loops)
Now have list comprehensions instead and the .join has beenr eplaced with an explicit string concatenation 
"""

def sum_sqaures(iters):
    out = [i ** 2 for i in range(iters)]
    return out 

def my_join(iters, string):
    out = ''
    for i in range(iters):
        out += ", "+ string
    return out 

def run_my_funcs(x,y):
    print(x,y)
    sum_squares(x)
    my_join(x,y)
    return 0 

run_my_funcs(10000000,"My string")