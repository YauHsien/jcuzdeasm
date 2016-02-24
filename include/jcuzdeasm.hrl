-ifndef(jcuzdeasm_hrl).
-define(jcuzdeasm_hrl, jcuzdeasm_hrl).

-record(parameter, {
	  type :: any() | [any()] | atom(),
	  name,
	  value :: any()
	 }).

-record(attribute, {
	  access :: public | protected | private | undefined,
	  abstract :: abstract | undefined,
	  level :: static | undefined,
	  final :: final | undefined,
	  type :: any() | [any()] | atom(),
	  name,
	  value :: any()
	 }).

-record(method, {
	  access :: public | protected | private | undefined,
	  abstract :: abstract | undefined,
	  level :: static | undefined,
	  final :: final | undefined,
	  name,
	  paramters = [] :: [#parameter{}],
	  throws = [] :: [any()],
	  return :: any()
	 }).

-record(interface, {
	  package = [] :: [string()],
	  imports = [] :: [any()],
	  access :: public | protected | private | undefined,
	  abstract :: abstract | undefined,
	  level :: static | undefined,
	  final :: final | undefined,
	  name,
	  inherits :: any(),
	  attributes = [] :: [#attribute{}],
	  methods = [] :: [#method{}]
	 }).

-record(class, {
	  package = [] :: [string()],
	  imports = [] :: [any()],
	  access :: public | protected | private | undefined,
	  abstract :: abstract | undefined,
	  level :: static | undefined,
	  final :: final | undefined,
	  name,
	  inherits :: any(),
	  implements = [] :: [#interface{}],
	  attributes = [] :: [#attribute{}],
	  methods = [] :: [#method{}]
	 }).

-record(profile, {
	  classes = [] :: [#class{} | #interface{}]
	 }).

-endif.
