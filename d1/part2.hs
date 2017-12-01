eqf a = if (fst a) == (snd a) then (fst a) else 0

cvt a = map cToI a
    where cToI n = read [n]

append [] ys = ys
append xs ys = (head xs) : (append (tail xs) ys)

splitAndMove l =
    append (drop midpoint l) (take midpoint l)
    where midpoint = quot (length l) 2

pairIt a =
    zip a (splitAndMove a)

compute ctnts = sum (map eqf (pairIt (cvt ctnts)))

main = do
    contents <- getLine
    print (compute contents)

