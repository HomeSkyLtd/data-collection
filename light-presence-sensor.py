#!/usr/bin/python
#--------------------------------------
#    ___  ___  _ ____          
#   / _ \/ _ \(_) __/__  __ __ 
#  / , _/ ___/ /\ \/ _ \/ // / 
# /_/|_/_/  /_/___/ .__/\_, /  
#                /_/   /___/   
#
#           bh1750.py
#  Read data from a digital light sensor.
#
# Author : Matt Hawkins
# Date   : 15/04/2015
#
# http://www.raspberrypi-spy.co.uk/
#
#--------------------------------------
import smbus
import time
import datetime
import RPi.GPIO as GPIO
import os.path

# Define some constants from the datasheet

DEVICE     = 0x23 # Default device I2C address

POWER_DOWN = 0x00 # No active state
POWER_ON   = 0x01 # Power on
RESET      = 0x07 # Reset data register value

# Start measurement at 4lx resolution. Time typically 16ms.
CONTINUOUS_LOW_RES_MODE = 0x13
# Start measurement at 1lx resolution. Time typically 120ms
CONTINUOUS_HIGH_RES_MODE_1 = 0x10
# Start measurement at 0.5lx resolution. Time typically 120ms
CONTINUOUS_HIGH_RES_MODE_2 = 0x11
# Start measurement at 1lx resolution. Time typically 120ms
# Device is automatically set to Power Down after measurement.
ONE_TIME_HIGH_RES_MODE_1 = 0x20
# Start measurement at 0.5lx resolution. Time typically 120ms
# Device is automatically set to Power Down after measurement.
ONE_TIME_HIGH_RES_MODE_2 = 0x21
# Start measurement at 1lx resolution. Time typically 120ms
# Device is automatically set to Power Down after measurement.
ONE_TIME_LOW_RES_MODE = 0x23

#bus = smbus.SMBus(0) # Rev 1 Pi uses 0
bus = smbus.SMBus(1)  # Rev 2 Pi uses 1

FILE = 'Snapshots.txt'

def convertToNumber(data):
	# Simple function to convert 2 bytes of data
	# into a decimal number
	return ((data[1] + (256 * data[0])) / 1.2)

def readLight(addr=DEVICE):
	data = bus.read_i2c_block_data(addr,ONE_TIME_HIGH_RES_MODE_1)
	return convertToNumber(data)

def main():
	GPIO.setmode(GPIO.BCM)
	GPIO.setup(4, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)
    # Write header if needed
    if not os.path.isfile(FILE):
        with open ('Snapshots.txt', 'a') as file_:
            file_.write('{:>10} {:>10} {:>12}\n'.format('light', 'presence', 'timestamp'))

	while True:
		presence = "0"
		if GPIO.input(4):
			presence = "1"
		# Get current time in UNIX timestamp
		now = int(time.time())
		# Compute message to write in file
        light = readLight()
		msg = "Light Level : " + str(light) + " lx | Presence: " + presence + " | Timestamp: " + str(now)
		with open ('Snapshots.txt', 'a') as file_:
			file_.write("{:>10.3f} {:>10} {:>12}\n".format(light, presence, now))
		print (msg)
		time.sleep(60.0)
  
if __name__=="__main__":
	main()
