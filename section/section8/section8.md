## Free Variables

Instructions: Write a box around the free variables in the following expres-
sions. For the bound variables, draw an arrow indicating it’s corresponding

binding occurrence :

Check the solutions, I don't think these are correct.

1. let x = y + 2 in x + 3
   in let f = (fun z -> y + 5)
   in let z = 8 in f z

   Comments:
   x is bound
   y (first) is free
   z is bound
   y (second) is free
   f is bound

2. fun z -> a + b + c + z

   Comments:
   a, b, c are free
   z is bound

3. x (fun x -> fun x -> x (fun x -> x))

   Comments:
   x (first) is free (looks like a function)
   x (all after first) are bound

4. let y = y + 2 in
   let g = (fun x -> y + 5)
   in (fun z -> z d) g

   Comments:
   y (first) is bound
   y (second) is free
   g (first and second) is bound
   y (third, in fun x -> y + 5) is bound to the first-line let y
   d is free
   z is bound

5. let x = 5 in
   x + (fun a -> z (fun z -> a y)) 5

   Comments:
   x is bound
   a (first) is bound
   a (second) is bound to the first a
   z (first) is free
   y is free

## Substitution

Instructions: Return the result of the following substitutions

1. ((x + 1) + (y + 2)) [x -> 3]

   (3 + 1) + (y + 2)

2. (let x = x + 1 in x + 2) [x -> 3]

   let x = 3 + 1 in x + 2

3. (let x = y + 2
   in let z = 5
   in z + x + j a)[x -> 5][j -> (fun x -> x * x)][a -> 4]

   (let x = y + 2
   in let z = 5
   in z + x + (fun x -> x _ x) 4)  
    [x -> 5]j -> (fun x -> x _ x)][a -> 4]

4. (let y = y + 2 in
   let g = (fun x -> y + 5)
   in (fun z -> z d) g))
   [y -> 8][d -> 4]

5. (let x = 5 in
   x + (fun a -> z (fun z -> a y)) 5)[a -> 2][z -> fun x -> x]

## Substitution Semantics-Evaluation Rules

Instructions: Use the Substitution Semantic Evaluation Rules to Derive the
Results of the Following expression

1.  let x = 8 in let y = x \* 3 in (fun x -> x + 4) 5 + x + y

    let y = x \_ 3 in (fun x -> x + 4) 5 + x + y
    [x -> 8]

    let y = 8 \_ 3 in (fun x -> x + 4) 5 + x + y
    [x -> 8]

    (fun x -> x + 4) 5 + x + y
    [x -> 8; y -> 8 * 3]

    (fun x -> x + 4) 5 + 8 + 8 _ 3
    [x -> 8; y -> 8 _ 3]

    fun x -> x + 4
    [x -> 5 + 8 + 8 * 3; y -> 8 * 3]

    5 + 8 + 8 \* 3 + 4

    ^ Should be 42, not 41

## Environment Semantics

1. Define an expression that evaluates to different values under lexical and
   dynamic semantic systems

2. Write out two derivations for the result of the expression: one using lexical
   semantics and the other using dynamic semantics.

3. Derive the evaluation for the result of the following expression
   let f = (fun x -> x \* 2) in
   let x = ref 42 in
   (x := !x - 10; !x) + f !x ;;
