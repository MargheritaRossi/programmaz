
let rec nth lst k =
    match (lst, k) with
    (x::xs, 0) -> x
    |(x::xs, k) -> (nth xs (k- 1));;

//calcola il quadrato
let square x =
    if (x=0) then 0
    elif(x=1) then 1
    else x*x;;

//calcola somma di quadrati estremi compresi
let rec sommaq x z =
    if  (x=z) then (square x)
    else (square x+sommaq (x+1) z);;

//somma di quadrati estremi esclusi
let rec sommaq2 x z =
    if((x=z)||((x+1)=(z))) then 0
    //elif((x+1)=(z))then 0
    else square(x+1) + sommaq2 (x+1) z;;

//calcola cubo
let cube x =
    match x with
    0-> 0
    |1-> 1
    |x-> if (x<0) then (-1*(x*x*x)) else x*x*x;;
   
//somma cubi estremicompresi
let rec sommac x z =
    if  (x=z) then (cube x)
    else (cube x+sommac (x+1) z);;

//somma cubi estremi esclusi
let rec sommac2 x z =
    if((x=z)||((x+1)=(z))) then 0
    //elif((x+1)=(z))then 0
    else cube(x+1) + sommac2 (x+1) z;;

//calcola la potenza 
let rec potenza b e =
    match (b, e) with
    0, 0 -> 1
    |0, 1 -> 0
    |1, 0 -> 1
    |_, 0 -> 1
    |1, 1 -> 1
    |_,1 -> b
    |_,_ -> b*potenza b (e-1);;


//stampa l'intervallo di una tupla
let rec stampainter ((a:int), (b:int)) =
    if(b=a) then a::[]
    else a::stampainter((a+1),b) ;;
    
//let rec sommapot (a, b) p =
 //   if(a=b) then 0
  //  elif ((a+1)=b) then 0
   // else (potenza a p) + sommapot ((a+1),b) p;;
    
//calcola la somma dei numeri in un intervallo (estremi esclusi) di una potenza data
let rec sommapot (a, b) p =
    if((a=b)||((a+1)=b)) then 0
    else (potenza (a+1) p) + sommapot ((a+1), b) p;;


//calcolare numero di uni nella rappresentazione decimale di un intero cifra data
let rec numerocifra x d =
    match x with
    x when x=d -> 1
    |x when (x<>d && x<10) -> 0
    |_ -> (numerocifra(x%10) d)+(numerocifra(x/10) d);;

//dispari
let dispari x = 
    if(x%2=1) then 1
    else 0;;

//numero cifre dispari in un numero
let rec cifre_d x =
    match x with
    x when x<10 -> dispari x
    |_ -> (cifre_d(x%10))+(cifre_d(x/10));;

//dispari2
let disparid x =
     x%2=1;;

//numero cifre dispari versione 2
let rec cif x=
    match x with
    x when x<10 && (disparid x) -> 1
    |_ -> (cifre_d(x%10))+(cifre_d(x/10));;

//funzione che somma il numero di cifre che compaiono nella rappresentazione decimale di un intero es. 234=2+3+4=9
let rec sommacifre x =
    match x with
    x when x <10 -> x
    |_ -> x%10+(sommacifre(x/10));;

//funzione che somma il numero di cifre che compaiono nella rappresentazione decimale di un intero es. 234=2,3,4=3
let rec quantecifre x=
    match x with
    x when x<10 -> 1
    |_ -> 1+quantecifre(x/10);;


//funzioneper trovare i divisori

let rec divIN x y =
    match x with
    x when x=y -> y::[]
    |_-> if(x%y=0) then y::(divIN x (y+1)) else divIN x (y+1);;

let rec div x = divIN x 1;;

//funzione che dice se un numero è primo
let prime x =
    if(div x = [1;x]) then true
    else false;;