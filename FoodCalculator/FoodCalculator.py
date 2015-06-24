'''
Created on Oct 27, 2011

@author: Josh Snider
'''
menu = {'12inchcheese':10.59,
              '16inchcheese':12.71,
              '12inchpepperoni':11.65,
              '16inchpepperoni':14.30}
print menu['12inchcheese']
money=input('How much money do you have to spend?')
foodprices=menu.values()
lowestprice=min(foodprices)
maxfood=money/lowestprice
maxfood=maxfood-maxfood%1
print maxfood
