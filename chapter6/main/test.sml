val num = 1;
val str = "hello";
val t = true;
val f = false;
val id = fn x => x;
val const = fn x => num;
val pair = (num, str);
val x = const f;
val y = id str;
val z = id t;
val left = #1 pair;
val right = #2 pair;
val ifexp = if f then (#1 pair) else 2;
val prim1 = prim(eq, 1, 2);
val prim2 = prim(add, 1, 2);
