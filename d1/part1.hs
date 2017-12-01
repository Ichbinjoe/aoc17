eqf a = if (fst a) == (snd a) then (fst a) else 0

cvt a = map cToI a
    where cToI n = read [n]

single a = tail a == []

pair a hd
     | single a = [(head a, hd)]
     | otherwise = (head a, a !! 1) : pair (tail a) hd

pairIt a =
    pair a (head a)

compute ctnts = sum (map eqf (pairIt (cvt ctnts)))

main = do
    contents <- getLine
    print (compute contents)
