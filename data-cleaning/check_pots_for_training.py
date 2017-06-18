#!/usr/bin/env python2.7

'''
Script to check and label some pots that contain plants to train a 
machine learning algorithm.

@author: Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)

'''

##########################################################################################
# import base packages
import os, sys, time, pandas, math
from time import gmtime, strftime
from datetime import datetime
import numpy as np
import subprocess
import random

##########################################################################################
def read_sv(csvname, sep=','):
    table=open(csvname,"r")
    table=[x.replace('\n','').split(sep) for x in table] 
    return table
##########################################################################################

data = read_sv('../data-raw/veggyraw',sep='\t')
# df = pandas.DataFrame(data)

## should install pygame and then I can directly do left or right!!



labeled=list()

counter=0
while counter<10:
	# select row
	tmp= random.sample(data, 1)
	theimage="/".join([".."]+[tmp[0][i] for i in (0,1,2)])

	# this is the image in case
	print(theimage)
		
	# open the graphic device
	p = subprocess.Popen(["eog" ,'--disable-gallery', '--new-instance',theimage], stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE )
	subprocess.call( str('wmctrl -a Terminal'),shell=True)

	# question about whether it is labeled
	answer=raw_input("(1=yes 0=no) is therea plant in " + tmp[0][4] +" ? ")
	labeled.append(tmp+[answer] )

	# close and count
	p.kill()
	counter=+counter

print ('finished!')



# for i in data:
# 	print (i)

# os.path.join('/my/root/directory', 'in', 'here')


#    p = subprocess.Popen(["eog" ,'--disable-gallery', '--new-instance',thefile], stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE )
#         # com=p.communicate() # does not work. because viewer is constant process it waits infinity
#         # print(com)
    
#     # If need help snapping window for better visualization. CAREFUL highly customized
#     if(windowsnap==True):
#         # waits 1 second for the image to be loaded 
#         time.sleep(0.5) 
    
#         # command to snap the window into my left screen. If not using same settings, comment out
#         # cmd=str("wmctrl -r " + os.path.basename(thefile) +' -e 0,0,0,1280,1400') # snaps one side
#         # #cmd=str("wmctrl -r " + os.path.basename(thefile) +' -e 0,0,0,2560,1400') # snaps to oposite screen
#         # subprocess.call(cmd,shell=True)
    
    
#     # command to put the terminal window to fron to be able to easily answer the questions 
#     subprocess.call( str('wmctrl -a Terminal'),shell=True)
   
#     # BIG CONTROLER

#     try:
#     ## Starts the questions
#     # question 1

#         tray=raw_input("Tray number [1:349] (.=repeated) (..= curate) > ")
        
#         if(tray==""):
#             theanswer=None
#         elif(tray==".." ):
#             theanswer=["curate"]*5
#         elif(tray=="."):
#             theanswer=["rep"]*5