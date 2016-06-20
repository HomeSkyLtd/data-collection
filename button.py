#!/usr/bin/python
"""This temporary script gets the value of button"""

import RPi.GPIO as GPIO
import time

INPUT = 6
OUTPUT = 5

class Lamp(object):
    """Class representing a Lamp"""
    def __init__(self):
        self.light_on = 0
    def get_light(self):
        """Get light"""
        return self.light_on
    def toggle_value(self):
        """Toggle value of Light_on"""
        time.sleep(0.1)
        if GPIO.input(INPUT):
            self.light_on = (self.light_on + 1) % 2
            GPIO.output(OUTPUT, self.light_on)

if __name__ == "__main__":
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(OUTPUT, GPIO.OUT)
    LAMP = Lamp()
    GPIO.add_event_detect(INPUT, GPIO.RISING, callback=LAMP.toggle_value(), bouncetime=100)
