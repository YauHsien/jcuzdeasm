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

    case { Cur, erlang:size(Bin),
	   , binary:match(Bin, [<<" class ">>])
	   , binary:match(Bin, [<<" interface ">>])
	   , binary:match(Bin, <<"/">>)
       , binary:match(Bin, <<"  ">>)
       , binary:match(Bin, <<";">>)
	 } of
	{undefined, _, ClassMatch, _, nomatch, _, _} when ClassMatch /= nomatch ->
	    {Prof, build_class(Bin)};

	{_, _, ClassMatch, _, nomatch, _, _} when ClassMatch /= nomatch ->
	    {Prof#profile{ classes= lists:append(Prof#profile.classes, [Cur]) }, build_class(Bin)};

	{undefined, _, _, InterfaceMatch, nomatch, _, _} when InterfaceMatch /= nomatch ->
	    {Prof, build_interface(Bin)};

	{_, _, _, InterfaceMatch, nomatch, _, _} when InterfaceMatch /= nomatch ->
	    {Prof#profile{ classes= lists:append(Prof#profile.classes, [Cur]) }, build_interface(Bin)};

    {_, Len, _, _, _, {0,2}, {Pos,1}} when Cur /= undefined andalso Len =:= Pos+1 ->
        {Prof, build_member(Cur, Bin)};

	{_, _, _, _, _, _, _} ->
	    {Prof, Cur}
    end.


build_class(Bin) ->
    element(
      1,
      lists:foldl(fun(Access, {Cls, undefined}) when Access == "public"
						     orelse Access == "protected"
						     orelse Access == "private"
						     ->
			  {Cls#class{ access= Access }, undefined};

		     ("abstract", {Cls, undefined}) ->
			  {Cls#class{ abstract= "abstract" }, undefined};

		     ("static", {Cls, undefined}) ->
			  {Cls#class{ level= "static" }, undefined};

		     ("final", {Cls, undefined}) ->
			  {Cls#class{ final= "final" }, undefined};

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

		     (Names, {Cls, implements}) ->
                 Names1 = string:tokens(Names, [$,]),
                 lists:foldl(fun(Name, {#class{}= Cls1, undefined}) ->
                        {Package, Name1} = get_qualified_name(Name),
                        {Cls1#class{
                            implements= [#interface{ package= Package, name= Name1 }
                                        |Cls1#class.implements]
                        }, undefined}
                     end,
                     {Cls, undefined},
                     Names1)

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
			 {Itf#interface{ access= Access }, undefined};

		    ("abstract", {Itf, undefined}) ->
			 {Itf#interface{ abstract= "abstract" }, undefined};

		    ("static", {Itf, undefined}) ->
			 {Itf#interface{ level= "static" }, undefined};

		    ("final", {Itf, undefined}) ->
			 {Itf#interface{ final= "final" }, undefined};

		    ("interface", {Itf, undefined}) ->
			 {Itf, interface};

		    (Name, {Itf, interface}) ->
			 {Package, Name1} = get_qualified_name(Name),
			 {Itf#interface{ package= Package, name= Name1 }, undefined};

		    ("extends", {Itf, undefined}) ->
			 {Itf, extends};

		    (Name, {Itf, extends}) ->
			 {Package, Name1} = get_qualified_name(Name),
			 {Itf#interface{
                 inherits= #interface{ package=Package, name=Name1 }
              }, undefined}

		 end,
		 {#interface{}, undefined},
		 string:tokens(binary:bin_to_list(Bin), [$\s,$\r])
		)
     ).

get_qualified_name(Name) ->
    Name1 = string:tokens(Name, [$., $,]), % . for qualifying words, ',' for multiple interface
    {lists:droplast(Name1), lists:last(Name1)}.


build_member(Cur, Bin) ->
    {IM, IA} = case Cur of
                   #class{} -> {#class.methods, #class.attributes};
                   #interface{} -> {#interface.methods, #interface.attributes}
               end,
    case {erlang:size(Bin), binary:match(Bin, <<");">>)} of
        {Len, Pos} when Len =:= Pos+2 ->
            setelement(IM, Cur, [ build_method(Cur, Bin) |element(IM,Cur)]);
        {_, _} ->
            setelement(IA, Cur, [ build_attribute(Cur, Bin) |element(IA,Cur)])
    end.

build_method(Cur, Bin) ->
    Cur1 = #method{},
    Method = 
        element(1,
            lists:foldl(build_with(Cur1),
                        {Cur1, undefined},
                        string:tokens(binary:bin_to_list(Bin), [$\s,$\r])
        )),
    Val = [ Method | element(?IMethods(Cur), Cur) ],
    setelement(?IMethods(Cur), Cur, Val).

build_attribute(Cur, Bin) ->
    Cur1 = #attribute{},
    Attribute =
        element(1,
            lists:foldl(build_with(Cur1),
                        {Cur1, undefined},
                        string:tokens(binary:bin_to_list(Bin), [$\s,$\r])
        )),
    Val = [ Attribute | element(?IAttributes(Cur), Cur) ],
    setelement(?IAttributes(Cur), Cur, Val).


build_with(Cur) ->
    
    fun(Access, {Cur, undefined}) when Access == "public"
                                       orelse Access == "private"
                                       orelse Access == "protected" ->
           {setelement(?IAccess(Cur), Cur, Access), qualified};
           
       ("abstract", {Cur, State}) -> when State == qualified
                                          orelse State == undefined ->
           {setelement(IAbstract(Cur), Cur, "abstract"), undefined};

       ("static", {Cur, State}) when State == qualified
                                     orelse State == undefined ->
           {setelement(ILevel(Cur), Cur, "static"), undefined};
           
       ("final", {Cur, State}) when State == qualified
                                    orelse State == undefined ->
           {setelement(IFinal(Cur), Cur, "final"), undefined};
           
       ("interface", {Cur, State}) when State == qualified
                                        orelse State == undefined ->
           {Cur, interface};
           
       ("class", {Cur, State}) when State == qualified
                                    orelse State == undefined ->
           {Cur, class};
           
       (Name, {Cur, State}) when State == qualified
                                 orelse State == undefined ->
           case re:run(Name, "\\()") of
               {match, [{_, 1}]} -> %% constructors
                   Cur1 = build_method_open_part(Cur, Name),
                   {Cur1, build_method_state(Name)};
               nomatch ->
                   {Package, Name} = get_qualified_name(TypeName),
                   Type = #type{ package= Package, name= Name },
                   {setelement(IType(Cur), Cur, Type), after_typing}
           end;
           
       (ItfName, {#interface{}= Cur, interface}) ->
           {Package, Name} = get_qualified_name(ItfName),
           {Cur#interface{ package= Package, name= Name }, named};
           
       (ClsName, {#class{}= Cur, class}) ->
           {Package, Name} = get_qualified_name(ClsName),
           {Cur#class{ package= Package, name= Name }, named};
           
       (AtbName, {#attribute{}= Cur, after_typing}) ->
           Name = string:join(string:tokens(AtbName, [$\;]), ""),
           {setelement(?IName(Cur), Cur, Name), attribute_end};
       
       (MbrName, {#method{}= Cur, after_typing}) ->
           Cur1 = build_method_open_part(Cur, MbrName),
           {Cur1, build_method_state(Name)};

       (Str, {#method{}= Cur, method_naming_open}) ->
           {Package, Name} = get_qualified_name(Str),
           Type = #type{ package= Package, name= Name },
           Param = #parameter{ type= Type },
           Params = Cur#method.parameters,
           {Cur#method{ parameters= [ Param | Params ] }, build_method_state(Str)};
           
       ("throws", {#method{}= Cur, method_naming_closed}) ->
           {Cur, throws};
           
       


build_method_open_part(#method{}= Method, Str) ->
    [Name1|Params] = string:tokens(Str, [$\(, $\,, $\), $\;]),
    {_, Name2} = get_qualified_name(Name1),
    Method1 = setelement(?IName(Method), Method, Name2),
    case Params of
        [] -> Method1;
        [Param] ->
            {Package, Name3} = get_qualified_name(Param),
            Type = #type{ package= Package, name= Name },
            NewParams = [ #parameter{ type= Type } | element(?IParameters(Method), Method) ],
            setelement(?IParameters(Method1), Method1, NewParams)
    end.
    
build_method_state(Str) ->
    case re:run(Str, "\\)$") of
        nomatch -> method_naming_open;
        {match, [{_, _}]} -> method_naming_closed
    end.


proc_profile(Prof) ->
    lists:map(fun(#class{}= Cls) ->
		      code_gen:class(Cls);
		 (#interface{}= Itf) ->
		      code_gen:interface(Itf)
	      end,
	      Prof#profile.classes).
