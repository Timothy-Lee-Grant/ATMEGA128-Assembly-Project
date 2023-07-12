
/*
Code was written by Timothy Lee Grant on October 13, 2021

This code will cause the bumpbot to go forward until it hits something. It will then push for 1 second, back up
turn towards the object it hit, go forward again until it hits again. Then repeats the same process.

PORT MAP
Port B, Pin 4 -> Output -> Right Motor Enable
Port B, Pin 5 -> Output -> Right Motor Direction
Port B, Pin 7 -> Output -> Left Motor Enable
Port B, Pin 6 -> Output -> Left Motor Direction
*/
#define F_CPU 16000000
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

int main(void)
{
	DDRB = 0b11110000;      // configure Port B pins for input/output
	PORTB = 0b11110000;     // set initial value for Port B outputs
	
	DDRD = 0b00000000;		//set PortD pins as input
	PORTD = 0b11111111;		//set pull
	// (initially, disable both motors)

	while (1) { // loop forever
		
		PORTB = 0b01100000;		//Move bot forward
		
		if(PIND == 0b11111110)	//Right Hit
		{
			_delay_ms(1000);	//keep moving forward for 1 second
			PORTB = 0b00000000;	//Moves bot backwards
			_delay_ms(500);		//wait for 500 ms
			PORTB = 0b01000000;	//turn to the right
			_delay_ms(500);		//wait for 500 ms
			
		}
		
		if(PIND == 0b11111101)	//Left Hit
		{
			_delay_ms(1000);	//keep moving forward for 1 second
			PORTB = 0b00000000;	//Moves bot backwards
			_delay_ms(500);		//wait for 500 ms
			PORTB = 0b00100000;	//turn to the left	
			_delay_ms(500);		//wait for 500 ms	
		}
		
		if(PIND == 0b11111100)	//both whiskers are hit
		{
			_delay_ms(1000);	//keep moving forward for 1 second
			PORTB = 0b00000000;	//Moves bot backwards
			_delay_ms(500);		//wait for 500 ms
			PORTB = 0b00100000;	//turn to the left
			_delay_ms(500);		//wait for 500 ms
		}
			
		
		

	}
}