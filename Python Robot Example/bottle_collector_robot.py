import RPi.GPIO as GPIO
import time

GPIO.setmode(GPIO.BCM)
GPIO.setwarnings(False)

Led_Rojo = 17
Led_Verde = 27
Led_Amarillo = 22
Led_Blanco = 4

A=1
B=1
C=1
D=1
E=1
F=1
iteration = 0

Pulsador_A = 5
Pulsador_B = 6
Pulsador_C = 13
Pulsador_D = 19

GPIO.setup(Led_Rojo, GPIO.OUT)
GPIO.setup(Led_Verde, GPIO.OUT)
GPIO.setup(Led_Blanco, GPIO.OUT)
GPIO.setup(Led_Amarillo, GPIO.OUT)

GPIO.setup(Pulsador_A, GPIO.IN)
GPIO.setup(Pulsador_B, GPIO.IN)
GPIO.setup(Pulsador_C, GPIO.IN)
GPIO.setup(Pulsador_D, GPIO.IN)

# The program starts, the robot is moving and turning

print (“Starting execution…”)

GPIO.output (Led_Rojo, 0)
GPIO.output (Led_Verde, 0)
GPIO.output (Led_Amarillo, 0)
GPIO.output (Led_Blanco, 0)

# The program runs 5 times and afterwards finish.

while iteration < 5:
    
    while (A):
        
        GPIO.output (Led_Rojo, 1)
        GPIO.output (Led_Verde, 0)
        print(“I am moving“)                         
        time.sleep(2)
        GPIO.output (Led_Rojo, 0)
        GPIO.output (Led_Verde, 1)
        print(“Turning”)
        time.sleep(2)
    
        inputValue = GPIO.input(Pulsador_C)
        if(A == inputValue):
            GPIO.output (Led_Verde, 0)
            print(“Stop turning as I can see a bottle”)
            GPIO.output(Led_Rojo, 1)
            time.sleep(0.3)
            print(“Going to the bottle")
            break
        
    while True:
        
        inputValue = GPIO.input(Pulsador_C)
        if(inputValue == True):
            GPIO.output(Led_Rojo, GPIO.LOW)
            print(“No longer see the bottle")
            time.sleep(3)
            break
    
    while (B):
    
        GPIO.output (Led_Rojo, 1)
        GPIO.output (Led_Verde, 0)
        print(“I am moving“)
        time.sleep(2)
        GPIO.output (Led_Rojo, 0)
        GPIO.output (Led_Verde, 1)
        print(“Turning“)
        time.sleep(2)
        inputValue = GPIO.input(Pulsador_C)
    
        if(B == inputValue):
            GPIO.output (Led_Verde, 0)
            print(“Stop turning“)
            print(“See the bootle“)
            GPIO.output(Led_Rojo, 1)
            print(“Going to the bottle“)
            time.sleep(7)
            GPIO.output(Led_Rojo, 0)
            print(“Behind the bottle”)
            time.sleep(0.3)
            break
    

#TOUCHING: GRIPPER IS OPEN
    
    while True:
    
        inputValue = GPIO.input(Pulsador_A)
        if(inputValue == True):
            GPIO.output(Led_Amarillo, GPIO.HIGH)
            print(“Opening the gripper“)
            time.sleep(6)
            break
    
    while True:
    
        inputValue = GPIO.input(Pulsador_A)
        if(inputValue == True):
            GPIO.output(Led_Amarillo, GPIO.LOW)
            GPIO.output(Led_Rojo, 1)
            print(“Going to the bottle“)
            time.sleep(7)
            GPIO.output(Led_Rojo, 0)
            print(“Behind the bottle“)
            time.sleep(7)
            break
    
    while True:
    
        inputValue = GPIO.input(Pulsador_A)
        if(inputValue == True):
            GPIO.output(Led_Amarillo, GPIO.HIGH)
            print(“Opening the gripper“)
            time.sleep(2)
            GPIO.output(Led_Amarillo, GPIO.LOW)
            print(“Bottle watched”)
            break

    while (C):
    
        GPIO.output (Led_Rojo, 1)
        GPIO.output (Led_Verde, 0)
        print(“I am moving“)                         
        time.sleep(3)
        GPIO.output (Led_Rojo, 0)
        GPIO.output (Led_Verde, 1)
        print(“Turning”)
        time.sleep(0.5)
    
        inputValue = GPIO.input(Pulsador_A)
    
        if(C == inputValue):
            GPIO.output(Led_Verde, 0)
            GPIO.output(Led_Rojo, 1)
            print(“Going to the bottle”)
            time.sleep(7)
            GPIO.output(Led_Rojo, 0)
            print(“Behind the bottle“)
            time.sleep(0.3)
            break

    while True:
    
        inputValue = GPIO.input(Pulsador_A)
        if(inputValue == True):
            GPIO.output(Led_Amarillo, GPIO.HIGH)
            print(“Open the gripper“)
            time.sleep(2)
            GPIO.output(Led_Amarillo, GPIO.LOW)
            print(“bottle catched“)
            break
        
    while (D):
    
        GPIO.output (Led_Rojo, 1)
        GPIO.output (Led_Verde, 0)
        print(“Moving”)                         
        time.sleep(3)
        GPIO.output (Led_Rojo, 0)
        GPIO.output (Led_Verde, 1)
        print(“Turning”)
        time.sleep(0.5)
    
        inputValue = GPIO.input(Pulsador_B)
        if(D == inputValue):
            GPIO.output(Led_Verde, 0)
            GPIO.output(Led_Rojo, 1)
            print(“Going to drop”)
            time.sleep (5)
            break

    while True:
    
        inputValue = GPIO.input(Pulsador_B)
        if(inputValue == True):
            GPIO.output(Led_Rojo, 1)
            print(“Going to bottle“)
            time.sleep(7)
            GPIO.output(Led_Rojo, 0)
            print(“Behind the bottle“)
            time.sleep(0.3)
            break
    
    while True:
    
        inputValue = GPIO.input(Pulsador_A)
        if(inputValue == True):
            GPIO.output(Led_Amarillo, GPIO.HIGH)
            print(“Open the gripper“)
            time.sleep(2)
            GPIO.output(Led_Amarillo, GPIO.LOW)
            print(“Bottle catched“)
            break

    while (E):

        GPIO.output (Led_Rojo, 1)
        GPIO.output (Led_Verde, 0)
        print(“Moving”)                         
        time.sleep(3)
        GPIO.output (Led_Rojo, 0)
        GPIO.output (Led_Verde, 1)
        print(“Turning”)
        time.sleep(0.5)
    
        inputValue = GPIO.input(Pulsador_B)
        if(E == inputValue):
            GPIO.output(Led_Verde, 0)
            GPIO.output(Led_Rojo, 1)
            time.sleep (0.3)
            print(“Going to the drop“)
            break

    while True:
    
        inputValue = GPIO.input(Pulsador_D)
        if(inputValue == True):
            GPIO.output(Led_Rojo, 0)
            GPIO.output(Led_Blanco, 1)
            GPIO.output(Led_Amarillo, 1)
            time.sleep(3)
            GPIO.output(Led_Blanco, 0)
            GPIO.output(Led_Amarillo, 1)
            print(“Dropping the bottle”)
            print(“Gripper open“)
            break

    inicio_temporizador = time.time()
    print (inicio_temporizador)

    while (F):
    
        tiempo_final = time.time()
        tiempo_ejecucion = tiempo_final - inicio_temporizador

        variable1 = GPIO.input(Pulsador_C)
        variable2 = GPIO.input(Pulsador_B)
        variable3 = GPIO.input(Pulsador_D)
    
        if (F == variable2):
            GPIO.output(Led_Rojo, 0)
                   GPIO.output(Led_Verde, 1)
            print("TURNING LEFT")
        
        elif (F == variable1):
            GPIO.output(Led_Rojo, 0)
                    GPIO.output(Led_Verde, 1)
            print("TURNING RIGHT")
        
        elif (F == variable3):
            GPIO.output(Led_Verde, 0)   
        
            GPIO.output(Led_Rojo, 1)
            print("MOVING")
     
        if (tiempo_ejecucion > 15):
        
            print ("Ha finalizado tu tiempo")
            break

    iteration = iteration + 1
    print ("El programa ha sido ejecutado :", + iteration ,"Programa terminado")

GPIO.cleanup()
    
