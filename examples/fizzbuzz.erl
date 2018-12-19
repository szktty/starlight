-module(fizzbuzz).

fizzbuzz(0) ->
    ok;
fizzbuzz(N) ->
    fizzbuzz1(N),
    fizzbuzz(N-1).

fizzbuzz1(N) ->
    case {N rem 3, N rem 5} of
        {0, 0} -> io:format("FizzBuzz");
        {0, _} -> io:format("Fizz");
        {_, 0} -> io:format("Buzz");
        _ -> io:format(integer_to_list(N))
    end.

main() ->
    fizzbuzz(15).
