#!/usr/bin/python
"""This script gets the light intensity and the presence of a person in a room
and save it in a file"""
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
import RPi.GPIO as GPIO
import os.path

# Define some constants from the datasheet

DEVICE = 0x23 # Default device I2C address

POWER_DOWN = 0x00 # No active state
POWER_ON = 0x01 # Power on
RESET = 0x07 # Reset data register value

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
BUS = smbus.SMBus(1)  # Rev 2 Pi uses 1

FILE = '/home/pi/Desktop/data-collection/snaps/Snapshots.txt'
INPUT = 6
OUTPUT = 16

class Lamp(object):
    """Class representing a Lamp"""
    def __init__(self):
        self.light_on = GPIO.input(INPUT)
        GPIO.output(OUTPUT, self.light_on)
    def get_light(self):
        """Get light"""
        return self.light_on
    def toggle_value(self, channel):
        """Toggle value of Light_on"""
        time.sleep(0.1)
        if GPIO.input(INPUT):
            self.light_on = (self.light_on + 1) % 2
            GPIO.output(OUTPUT, self.light_on)

def convert_to_number(data):
    """Simple function to convert 2 bytes of data into a decimal number"""
    return (data[1] + (256 * data[0])) / 1.2

def read_light(addr=DEVICE):
    """Reads light intensity from bus"""
    data = BUS.read_i2c_block_data(addr, ONE_TIME_HIGH_RES_MODE_1)
    return convert_to_number(data)

def main():
    """Main function to loop and gets data every 60 seconds"""
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(4, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)
    GPIO.setup(OUTPUT, GPIO.OUT)
    GPIO.setup(INPUT, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)
    LAMP = Lamp()
    GPIO.add_event_detect(INPUT, GPIO.RISING, callback=LAMP.toggle_value, bouncetime=100)
    # Write header if needed
    if not os.path.isfile(FILE):
        with open(FILE, 'a') as file_:
            file_.write('{:>10} {:>10} {:>10} {:>12}\n'.format('light', 'presence', 'light_on', 'timestamp'))

    while True:
        presence = "0"
        if GPIO.input(4):
            presence = "1"
        # Get current time in UNIX timestamp
        now = int(time.time())
        # Compute message to write in file
        light = read_light()
        msg = "Light Level : " + str(light)
        msg += " lx | Presence: " + presence
        msg += " | Timestamp: " + str(now)
        msg += " | Light_on: " + str(LAMP.get_light())

        with open(FILE, 'a') as file_:
            file_.write("{:>10.3f} {:>10} {:>10} {:>12}\n".format(light, presence, LAMP.get_light(), now))
        print msg
        time.sleep(60.0)

if __name__ == "__main__":
    main()
