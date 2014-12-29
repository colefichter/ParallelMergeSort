-module(parallel_merge_sort).

%% parallel_merge_sort: parallel_merge_sort library's entry point.

%-export([merge_sort/1]).
-compile([export_all]).

 

%%------------------------------------------------------------------
%% Client API
%%------------------------------------------------------------------

merge_sort(List) ->
	N = length(List),
	lists:flatten(merge_sort(List, N)).


%%------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------

merge_sort(List, N) when N =< 1 -> 
	%Base case to terminate recusion. A list of length 1 is sorted by definition.
	List;
merge_sort(List, N) when N > 1 ->
	MiddleIndex = N div 2, %Note: integer division uses 'div' while floating-point division uses'/'.
	{Left, Right} = lists:split(MiddleIndex, List),
	Left2 = merge_sort(Left),
	Right2 = merge_sort(Right),
	merge(Left2, Right2).

% Merge two individual items into a sorted list (in ascending order) of length 2.
% merge(Left, Right) when Left > Right ->
% 	%The extra list is a trick to ensure the result is a well-formed list. See "Defining Lists" in
% 	%Joe Armstrong's Programming Erlang (1st ed.), Page 28.
% 	[Right | [Left]]; 
% merge(Left, Right) when Left =< Right ->
% 	[Left | [Right]].

%% Merge two SORTED lists of arbitrary length into a single list.
merge(Left, Right) ->
	merge(Left, Right, []).

merge([], [], Result) ->
	lists:flatten(lists:reverse(Result));

merge([], Right, Result) ->
	merge([], [], [Right|Result]);
merge(Left, [], Result) ->
	merge([], [], [Left|Result]);

merge([L1|LRest], [R1|RRest], Result) when L1 =< R1 ->
	merge(LRest, [R1|RRest], [L1|Result]);
merge([L1|LRest], [R1|RRest], Result) when L1 > R1 ->
	merge([L1|LRest], RRest, [R1|Result]).

 
%%------------------------------------------------------------------
%% Unit tests
%%------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



%% TODO: add tests for other types!
merge_test_() ->
    [
    	?_assertEqual([2], merge([], [2])),
    	?_assertEqual([2], merge([2], [])),
    	?_assertEqual([1, 2], merge([1], [2])),
    	?_assertEqual([1, 2], merge([2], [1])),
    	?_assertEqual([0, 1], merge([1], [0])),
    	?_assertEqual([0, 1], merge([0], [1])),
    	?_assertEqual([-1, 0], merge([-1], [0])),
    	?_assertEqual([-1, 0], merge([0], [-1])),
    	?_assertEqual([-1, 1], merge([-1], [1])),
    	?_assertEqual([-1, 1], merge([1], [-1])),
    	?_assertEqual([1,2,3], merge([1,2], [3])),
    	?_assertEqual([1,1,2,3], merge([1,2], [1,3])),
    	?_assertEqual([1,2,3,4,5,6,7,8,9,10], merge([1,2,4,5,6,7,8,10], [3,9]))
    ].

merge_sort_test_() ->
	X = [12,19,24,13,8,15,23,17,12,15,4,6,18,4,14,6,12,11,1,15,12,11,8,2,15,25,9,5,6,
		 1,23,21,21,9,7,21,1,1,2,2,25,15,16,10,17,22,23,20,15,8,18,11,20,2,10,15,17,
		 17,2,7,16,1,24,21,17,11,12,1,16,24,7,25,24,22,7,8,19,15,6,3,4,5,6,24,20,9,7,
		 12,1,15,24,24,8,25,9,15,11,1,3,15],
 	Y = lists:sort(X),
	[
		?_assertEqual([1,2], merge_sort([1,2])),
		?_assertEqual([1,2], merge_sort([2,1])),
		?_assertEqual([1,2,3,4], merge_sort([1,2,3,4])),
		?_assertEqual([1,2,3,4], merge_sort([4,3,2,1])),
		?_assertEqual([1,2,3,4], merge_sort([2,1,4,3])),
		?_assertEqual([1,2,2,3], merge_sort([2,1,2,3])),
		?_assertEqual([1,2,3,4,5], merge_sort([1,2,3,4,5])),
		?_assertEqual([1,2,3,4,5], merge_sort([5,4,3,2,1])),
		?_assertEqual([1,2,2,3,4,5], merge_sort([5,2,4,3,2,1])),
		?_assertEqual(Y, merge_sort(X)) %Larger list.
	].

-endif.