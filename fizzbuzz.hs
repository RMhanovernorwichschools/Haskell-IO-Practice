{- 
    Write an interactive program that does the FizzBuzz problem that was in
    an earlier homework assignment. Here is an example of how the program should work:

    How many numbers shall we print? 25
    For multiples of what number shall we print 'Fizz'? 3
    For multiples of what number shall we print 'Buzz'? 5
    1
    2
    Fizz
    4
    Buzz
    Fizz
    7
    8
    Fizz
    Buzz
    11
    Fizz
    13
    14
    FizzBuzz
    16
    17
    Fizz
    19
    Buzz
    Fizz
    22
    23
    Fizz
    Buzz
    
    Tips:
    
    * Example of getting an Integer (as opposed to a String) from the user: 
        numVariable  <- readLn
      (Clarification: It gets whatever data type is required.)
    
    * Use your fizzBuzz function from the lists homework assignment. You can
      copy that code into this file.
      
    * Use mapM_ with putStrLn to print the result of calling fizzBuzz.

-}

fizzBuzz n f b = [(fbCheck x f b)| x <- [1..n]]
fbCheck n f b = if (mod n f ==0) && (mod n b==0) then "FizzBuzz" else if (mod n f ==0) then "Fizz" else if (mod n b ==0) then "Buzz" else show n

main = do
    putStr "How many numbers shall we print? "
    total <- readLn
    putStr "For multiples of what number shall we print 'Fizz'? "
    fMul <- readLn
    putStr "For multiples of what number shall we print 'Buzz'? "
    bMul <- readLn
    mapM_ (putStrLn) (fizzBuzz total fMul bMul)

