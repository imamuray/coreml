val num = 1;
val str = "hello";
val constT = true;
val constF = false;
val id = fn x => x;
val const = fn x => num;
val pair = (num, str);
val x = const constF;
val y = id str;
val z = id constT;
val left = #1 pair;
val right = #2 pair;
val ifexp = if constF then (#1 pair) else 2;
val prim1 = eq(1, 2);
val prim2 = add(1, 2);
fun addone x = add(1, x);
val two = addone 1;
fun f x = if eq(x, 1) then 1 else mul(x, f sub(x, 1));
val result = f 10;
val printTest = print(str);