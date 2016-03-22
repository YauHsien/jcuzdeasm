-ifndef(jcuzdeasm_hrl).
-define(jcuzdeasm_hrl, jcuzdeasm_hrl).

-record(type, {
        package = [] :: [string()],
        name :: string()
    }).

-record(parameter, {
	  type :: type(),
	  name :: string(),
	  value :: any()
	 }).

-record(attribute, {
	  access :: "public" | "protected" | "private",
	  abstract :: "abstract",
	  level :: "static",
	  final :: "final",
	  type :: type(),
	  name :: string(),
	  value :: any()
	 }).

-record(method, {
	  access :: "public" | "protected" | "private",
	  abstract :: "abstract",
	  level :: "static",
	  final :: "final",
      type :: type(),
	  name :: string(),
	  paramters = [] :: [#parameter{}],
	  throws = [] :: [type()]
	 }).

-record(interface, {
	  package = [] :: [string()],
	  imports = [] :: [any()],
	  access :: "public" | "protected" | "private",
	  abstract :: "abstract",
	  level :: "static",
	  final :: "final",
	  name :: string(),
	  inherits :: any(),
	  attributes = [] :: [#attribute{}],
	  methods = [] :: [#method{}]
	 }).

-record(class, {
	  package = [] :: [string()],
	  imports = [] :: [any()],
	  access :: "public" | "protected" | "private",
	  abstract :: "abstract",
	  level :: "static",
	  final :: "final",
	  name :: string(),
	  inherits :: any(),
	  implements = [] :: [#interface{}],
	  attributes = [] :: [#attribute{}],
	  methods = [] :: [#method{}]
	 }).

-record(profile, {
	  classes = [] :: [#class{} | #interface{}]
	 }).
     
-define(IPackage(R), case R of
                         #type{} -> #type.package;
                         #interface{} -> #interface.package;
                         #class{} -> #class.package
                     end).
                     
-define(IName(R), case R of
                      #type{} -> #type.name;
                      #parameter{} -> #parameter.name;
                      #attribute{} -> #attribute.name;
                      #method{} -> #method.name;
                      #interface{} -> #interface.name;
                      #class{} -> #class.name
                  end).

-define(IImports(R), case R of
                         #interface{} -> #interface.imports;
                         #class{} -> #class.imports
                     end).

-define(IAccess(R), case R of
                        #attribute{} -> #attribute.access;
                        #method{} -> #method.access;
                        #interface{} -> #interface.access;
                        #class{} -> #class.access
                    end).

-define(IAbstract(R), case R of
                          #attribute{} -> #attribute.abstract;
                          #method{} -> #method.abstract;
                          #interface{} -> #interface.abstract;
                          #class{} -> #class.abstract
                      end).
                      
-define(ILevel(R), case R of
                       #attribute{} -> #attribute.level;
                       #method{} -> #method.level;
                       #interface{} -> #interface.level;
                       #class{} -> #class.level
                   end).

-define(IFinal(R), case R of
                       #attribute{} -> #attribute.final;
                       #method{} -> #method.final;
                       #interface{} -> #interface.final;
                       #class{} -> #class.final
                   end).
                   
-define(IType(R), case R of
                      #parameter{} -> #parameter.type;
                      #attribute{} -> #attribute.type;
                      #method{} -> #method.type
                  end).

-define(IValue(R), case R of
                       #parameter{} -> #parameter.value;
                       #attribute{} -> #attribute.value
                   end).

-define(IParameters(_R), #method.parameters).
-define(IThrows(_R), #method.throws).

-define(IInherits(R), case R of
                          #interface{} -> #interface.inherits;
                          #class{} -> #class.inherits
                      end).

-define(IImplements(_R), #class.implements).

-define(IAttributes(R), case R of
                            #interface{} -> #interface.attributes;
                            #class{} -> #class.attributes
                        end).
                        
-define(IMethods(R), case R of
                         #interface{} -> #interface.methods;
                         #class{} -> #class.methods
                     end).
                     
-define(IClasses(_R), #profile.classes).

-endif.
