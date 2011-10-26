%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_element).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{get_id, 1},        % Construct the id representing the given element.
     {get_path, 1}       % Return the path that should be used (appended to hostname for URI) for referring to the given element.
    ];
behaviour_info(_Other) -> undefined.
