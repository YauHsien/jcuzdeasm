-module(javasm).
-export([ to_code/1 ]).
-include("../include/jcuzdeasm.hrl").


to_code(#profile{}= Prof) ->
    proc_profile(Prof);

to_code(File) ->
    case filelib:is_file(File) of
	true ->
	    {ok, Bin} = file:read_file(File),
	    to_code(Bin);

	false ->
	    {Prof, Any} = lists:foldl(fun proc_line/2, {#profile{}, undefined}, to_lines(File)),
	    to_code(Prof#profile{ classes= lists:append(Prof#profile.classes, [Any]) })
    end.


to_lines(Data) ->
    re:split(Data, <<"\n">>).


proc_line(<<"class ", _/binary>>= Bin, {Prof, undefined}) ->
    {Prof, build_class(Bin)};

proc_line(<<"class ", _/binary>>= Bin, {Prof, Cur}) ->
    {Prof#profile{ classes= lists:appned(Prof#profile.classes, [Cur]) }, build_class(Bin)};

proc_line(<<"interface ", _/binary>>= Bin, {Prof, undefined}) ->
    {Prof, build_interface(Bin)};

proc_line(<<"interface ", _/binary>>= Bin, {Prof, Cur}) ->
    {Prof#profile{ classes= lists:append(Prof#profile.classes, [Cur]) }, build_interface(Bin)};

proc_line(Bin, {Prof, Cur}) ->

    case { Cur
	   , binary:match(Bin, [<<" class ">>, <<" class">>])
	   , binary:match(Bin, [<<" interface ">>, <<" interface">>])
	   , binary:match(Bin, <<"/">>)
	 } of
	{undefined, ClassMatch, _, nomatch} when ClassMatch /= nomatch ->
	    {Prof, build_class(Bin)};

	{_, ClassMatch, _, nomatch} when ClassMatch /= nomatch ->
	    {Prof#profile{ classes= lists:append(Prof#profile.classes, [Cur]) }, build_class(Bin)};

	{undefined, _, InterfaceMatch, nomatch} when InterfaceMatch /= nomatch ->
	    {Prof, build_interface(Bin)};

	{_, _, InterfaceMatch, nomatch} when InterfaceMatch /= nomatch ->
	    {Prof#profile{ classes= lists:append(Prof#profile.classes, [Cur]) }, build_interface(Bin)};

	{_, _, _, _} ->
	    {Prof, Cur}
    end.


build_class(Bin) ->
    element(
      1,
      lists:foldl(fun(Access, {Cls, undefined}) when Access == "public"
						     orelse Access == "protected"
						     orelse Access == "private"
						     ->
			  {Cls#class{ access= list_to_atom(Access) }, undefined};

		     ("abstract", {Cls, undefined}) ->
			  {Cls#class{ abstract= abstract }, undefined};

		     ("static", {Cls, undefined}) ->
			  {Cls#class{ level= static }, undefined};

		     ("final", {Cls, undefined}) ->
			  {Cls#class{ final= final }, undefined};

		     ("class", {Cls, undefined}) ->
			  {Cls, class};

		     (Name, {Cls, class}) ->
			  {Package, Name1} = get_qualified_name(Name),
			  {Cls#class{ package= Package, name= Name1 }, undefined};

		     ("extends", {Cls, undefined}) ->
			  {Cls, extends};

		     (Name, {Cls, extends}) ->
			  {Package, Name1} = get_qualified_name(Name),
			  {Cls#class{
			     inherits= #class{ package= Package, name= Name1 }
			    },
			   undefined};

		     ("implements", {Cls, undefined}) ->
			  {Cls, implements};

		     (Name, {Cls, implements}) ->
			  {Package, Name1} = get_qualified_name(Name),
			  {Cls#class{
			     implements=
				 lists:append(
				   Cls#class.implements,
				   [#interface{ package= Package, name= Name1 }]
				  )},
			   case lists:last(Name) of
			       $, ->
				   implements;
			       _ ->
				   undefined
			   end}

		  end,
		  {#class{}, undefined},
		  string:tokens(binary:bin_to_list(Bin), [$\s,$\r])
		 )
     ).


build_interface(Bin) ->
    element(
      1,
      lists:foldl(fun(Access, {Itf, undefined}) when Access == "public"
						    orelse Access == "protected"
						    orelse Access == "private"
						    ->
			 {Itf#interface{ access= list_to_atom(Access) }, undefined};

		    ("abstract", {Itf, undefined}) ->
			 {Itf#interface{ abstract= abstract }, undefined};

		    ("static", {Itf, undefined}) ->
			 {Itf#interface{ level= static }, undefined};

		    ("final", {Itf, undefined}) ->
			 {Itf#interface{ final= final }, undefined};

		    ("interface", {Itf, undefined}) ->
			 {Itf, interface};

		    (Name, {Itf, interface}) ->
			 {Package, Name1} = get_qualified_name(Name),
			 {Itf#interface{ package= Package, name= Name1 }, undefined};

		    ("extends", {Itf, undefined}) ->
			 {Itf, extends};

		    (Name, {Itf, extends}) ->
			 {Package, Name1} = get_qualified_name(Name),
			 {Itf#interface{ package= Package, name= Name1 }, undefined}

		 end,
		 {#interface{}, undefined},
		 string:tokens(binary:bin_to_list(Bin), [$\s,$\r])
		)
     ).

get_qualified_name(Name) ->
    Name1 = string:tokens(Name, [$., $,]), % . for qualifying words, ',' for multiple interface
    {lists:droplast(Name1), lists:last(Name1)}.


proc_profile(Prof) ->
    lists:map(fun(#class{}= Cls) ->
		      code_gen:class(Cls);
		 (#interface{}= Itf) ->
		      code_gen:interface(Itf)
	      end,
	      Prof#profile.classes).
