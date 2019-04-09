-module(onp).
-export([onp/1, onpSt/2]).

onpSt(["+" | T], [A | [B | C]]) -> onpSt(T, [B + A | C]);
onpSt(["-" | T], [A | [B | C]]) -> onpSt(T, [B - A | C]);
onpSt(["*" | T], [A | [B | C]]) -> onpSt(T, [B * A | C]);
onpSt(["/" | T], [A | [B | C]]) -> onpSt(T, [B / A | C]);
onpSt(["sqrt" | T], [A | B]) -> onpSt(T, [math:sqrt(A) | B]);
onpSt(["pow" | T], [A | [B | C]]) -> onpSt(T, [math:pow(B, A) | C]);
onpSt(["sin" | T], [A | B]) -> onpSt(T, [math:sin(A) | B]);
onpSt(["cos" | T], [A | B]) -> onpSt(T, [math:cos(A) | B]);

onpSt([H | T], Stack) -> case lists:member($., H) of
                           true -> onpSt(T, [list_to_float(H) | Stack]);
                           false -> onpSt(T, [list_to_integer(H) | Stack])
                         end;

onpSt([], Stack) -> hd(Stack).

onp(Str) -> onpSt(string:tokens(Str, " "), []).