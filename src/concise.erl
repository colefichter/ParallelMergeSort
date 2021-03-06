-module(concise).

-export([merge_sort/1]).

%%------------------------------------------------------------------
%% Client API
%%------------------------------------------------------------------

%%This version uses built-in libraries to reduce the amount of code we have to write.
%% (Also benefits from performance increases of OTP libraries.)

merge_sort([]) -> [];
merge_sort([Head]) -> [Head];
merge_sort(List) ->
	MiddleIndex = length(List) div 2, %Note: integer division uses 'div' while floating-point division uses'/'.
	{Left, Right} = lists:split(MiddleIndex, List),
	lists:merge(merge_sort(Left), merge_sort(Right)).
 
%%------------------------------------------------------------------
%% Unit tests
%%------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

merge_sort_test_() ->
	X = [12,19,24,13,8,15,23,17,12,15,4,6,18,4,14,6,12,11,1,15,12,11,8,2,15,25,9,5,6,
		 1,23,21,21,9,7,21,1,1,2,2,25,15,16,10,17,22,23,20,15,8,18,11,20,2,10,15,17,
		 17,2,7,16,1,24,21,17,11,12,1,16,24,7,25,24,22,7,8,19,15,6,3,4,5,6,24,20,9,7,
		 12,1,15,24,24,8,25,9,15,11,1,3,15],
 	Y = lists:sort(X),
	[
		?_assertEqual([], merge_sort([])),
		?_assertEqual([1], merge_sort([1])),
		?_assertEqual([1,2], merge_sort([1,2])),
		?_assertEqual([1,2], merge_sort([2,1])),
		?_assertEqual([1,2,3,4], merge_sort([1,2,3,4])),
		?_assertEqual([1,2,3,4], merge_sort([4,3,2,1])),
		?_assertEqual([1,2,3,4], merge_sort([2,1,4,3])),
		?_assertEqual([1,2,2,3], merge_sort([2,1,2,3])),
		?_assertEqual([1,2,3,4,5], merge_sort([1,2,3,4,5])),
		?_assertEqual([1,2,3,4,5], merge_sort([5,4,3,2,1])),
		?_assertEqual([1,2,2,3,4,5], merge_sort([5,2,4,3,2,1])),
		?_assertEqual(Y, merge_sort(X)), %Larger list.
		?_assertEqual(["a", "b", "c", "x", "y", "z"], merge_sort(["x", "a", "z", "y", "c", "b"]))
	].

-endif.