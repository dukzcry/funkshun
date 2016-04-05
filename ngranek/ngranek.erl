-module(ngranek).

-author('A.V. Lukyanov <lomka@gero.in>').

-include_lib("wx/include/gl.hrl").

%% external eunit tests
-compile(export_all).

-record(vec,{x::number(),y::number()}).
-record(vec_pair,{left::#vec{},right::#vec{}}).

-define(CRIMSON,{220,20,60}).
-define(PURPLE,{128,0,128}).

-define(UNMELLOW,{255,255,120}).
-define(LIME,{50,205,50}).

-define(BABY,{137,207,240}).
-define(CADET,{29,41,81}).

-record(data,{screen=[800,600],pov=[[0,-240],[0,-140]],step=20,objs=[[?CRIMSON,[[[-100,-100],[-100,100]],[[100,-100],[100,100]]]],
								     [?PURPLE,[[[-100,-100],[100,-100]],[[-100,100],[100,100]]]]
								]}).

-spec list_to_vec_pair(List) -> #vec_pair{} when List::[[number()]].
list_to_vec_pair([[X1,Y1],[X2,Y2]]) ->
    #vec_pair{left=#vec{x=X1,y=Y1},right=#vec{x=X2,y=Y2}}.

%% vector algebra
-spec dot_product(V1,V2) -> number() when V1::#vec{},
					  V2::#vec{}.
dot_product(#vec{x=X1,y=Y1},#vec{x=X2,y=Y2}) ->
    X1*X2+Y1*Y2.

-spec vec_len(V) -> number() when V::#vec{}.
vec_len(V) ->
    math:sqrt(dot_product(V,V)).

-spec vec_norm(V) -> #vec{} when V::#vec{}.
vec_norm(V = #vec{x=X,y=Y}) ->
    Len = vec_len(V),
    #vec{x=X/Len,y=Y/Len}.

-spec vec_op(Op,V1,V2) -> #vec{} when Op::'+'|'-',
				      V1::#vec{},
				      V2::#vec{}.
vec_op(Op,#vec{x=X2,y=Y2},#vec{x=X1,y=Y1}) ->
    case Op of
	'+' ->
	    #vec{x=X2+X1,y=Y2+Y1};
	'-' ->
	    #vec{x=X2-X1,y=Y2-Y1}
    end.

-spec vec_scal_mul(V,N) -> #vec{} when V::#vec{},
				       N::number().
vec_scal_mul(#vec{x=X,y=Y},N) ->
    #vec{x=X*N,y=Y*N}.

-spec vec_perp(V) -> #vec{} when V::#vec{}.
vec_perp(#vec{x=X,y=Y}) ->
    #vec{x=Y,y=-X}.

-spec vec_rot(V,D,An) -> #vec{} when V::#vec{},
				     D::#vec{},
				     An::number().
vec_rot(V,D = #vec{x=Px,y=Py},An) ->
    Can = math:cos(An),
    San = math:sin(An),
    #vec{x=Xpx,y=Ypy} = vec_op('-',V,D),
    #vec{x=Px+Xpx*Can-Ypy*San,y=Py+Ypy*Can+Xpx*San}.

-spec vec_angle(V1,V2) -> number() when V1::#vec{},
					V2::#vec{}.
vec_angle(#vec{x=X1,y=Y1},#vec{x=X2,y=Y2}) ->
    abs(math:atan2(X1*Y2-X2*Y1,X1*X2+Y1*Y2)).

-spec get_side_dots(PovEnd,Ppn,Hwidth) -> #vec_pair{} when PovEnd::#vec{},
							   Ppn::#vec{},
							   Hwidth::number().
get_side_dots(PovEnd,Ppn,Hwidth) ->
    Ppv = vec_scal_mul(Ppn,Hwidth),
    #vec_pair{left=vec_op('-',PovEnd,Ppv),right=vec_op('+',PovEnd,Ppv)}.

-spec get_sides(PovBeg,Sides) -> #vec_pair{} when PovBeg::#vec{},
						  Sides::#vec_pair{}.
get_sides(PovBeg,#vec_pair{left=Side1Dot,right=Side2Dot}) ->
    #vec_pair{left=vec_op('-',Side1Dot,PovBeg),right=vec_op('-',Side2Dot,PovBeg)}.

-spec get_yf(V1,D1,V2,D2) -> no_return() | fun((X) -> number()) when V1::#vec{},
								     D1::#vec{},
								     V2::#vec{},
								     D2::#vec{},
								     X::number().
get_yf(V1 = #vec{x=V1x,y=V1y},D1 = #vec{x=D1x,y=D1y},V2,D2) ->
    if V1x == 0 ->
	    get_yf(V2,D2,V1,D1);
       true ->
	    %% canonical line equation
	    fun(X) -> V1y*(X-D1x)/V1x+D1y end
    end.

-spec get_intersect(V1,D1,V2,D2) -> number() when V1::#vec{},
						  D1::#vec{},
						  V2::#vec{},
						  D2::#vec{}.
get_intersect(#vec{x=V1x,y=V1y},#vec{x=D1x,y=D1y},#vec{x=V2x,y=V2y},#vec{x=D2x,y=D2y}) ->
    P2p3 = V1y*V2x,
    P1p4 = V1x*V2y,
    (((D2y-D1y)*(V1x*V2x)-P1p4*D2x)+P2p3*D1x)/(P2p3-P1p4).

-spec find_intersect(Beg,End,PovBeg,Side) -> #vec{} | [] when Beg::#vec{},
							      End::#vec{},
							      PovBeg::#vec{},
							      Side::#vec{}.
find_intersect(Beg,End,PovBeg,Side) ->
    Vface = vec_op('-',End,Beg),
    X = get_intersect(Side,PovBeg,Vface,Beg),
    Yf = get_yf(Side,PovBeg,Vface,Beg),
    Dot = #vec{x=X,y=Yf(X)},
    Vlen = vec_len(Vface),
    %% between A & B
    case vec_len(vec_op('-',Dot,Beg)) =< Vlen
	andalso vec_len(vec_op('-',Dot,End)) =< Vlen
    of
	true ->
	    Dot;
	false ->
	    []
    end.

-spec choose_intersect(Beg,End,PovBeg,Side1,Side2) -> no_return() | #vec_pair{} when Beg::#vec{},
										     End::#vec{},
										     PovBeg::#vec{},
										     Side1::#vec{},
										     Side2::#vec{}.
choose_intersect(Beg,End,PovBeg,Side1,Side2) ->
    Intersect = find_intersect(Beg,End,PovBeg,Side1),
    case Intersect == [] of
	true ->
	    #vec_pair{left=find_intersect(Beg,End,PovBeg,Side2),right=Side2};
	false ->
	    #vec_pair{left=Intersect,right=Side1}
    end.

-spec get_vpnf(Pv,Ppv,PovEnd) -> fun((Dot) -> #vec{}) when Pv::#vec{},
							   Ppv::#vec{},
							   PovEnd::#vec{},
							   Dot::#vec{}.
get_vpnf(Pv,Ppv,PovEnd) ->
    fun(Dot) ->
	    Ppx = get_intersect(Ppv,PovEnd,Pv,Dot),
	    Yf = get_yf(Ppv,PovEnd,Pv,Dot),
	    %% vec who's normal to the plane
	    vec_op('-',Dot,#vec{x=Ppx,y=Yf(Ppx)})
    end.

-spec recalc_plane(Pov,Hwidth) -> {#vec_pair{},#vec_pair{},#vec_pair{}} when Pov::#vec_pair{},
									     Hwidth::number().
recalc_plane(#vec_pair{left=Beg,right=End},Hwidth) ->
    Pv = vec_op('-',End,Beg),
    Ppn = vec_perp(vec_norm(Pv)),
    SideDots = get_side_dots(End,Ppn,Hwidth),
    {#vec_pair{left=Pv,right=Ppn},SideDots,get_sides(Beg,SideDots)}.

-spec gen_faces(Begs,Ends,Color,Pvs,Pov,Heights,SideDot1) -> [] | {number(),Color,[#vec{}]} when Begs::#vec_pair{},
												 Ends::#vec_pair{},
												 Color::{number(),number(),number()},
												 Pvs::#vec_pair{},
												 Pov::#vec_pair{},
												 Heights::{number(),number()},
												 SideDot1::#vec{}.
% choose_intersect failed
gen_faces(_,[],_,_,_,_,_) ->
    [];
gen_faces(#vec_pair{left=Beg,right=Begv},#vec_pair{left=End,right=Endv},Color,#vec_pair{left=Pv,right=Ppv},#vec_pair{left=PovBeg,right=PovEnd},{Height,Hheight},SideDot1) ->
    Begx = get_intersect(Begv,PovBeg,Ppv,PovEnd),
    Endx = get_intersect(Endv,PovBeg,Ppv,PovEnd),
    BegYf = get_yf(Begv,PovBeg,Ppv,PovEnd),
    EndYf = get_yf(Endv,PovBeg,Ppv,PovEnd),
    BegDot = #vec{x=Begx,y=BegYf(Begx)},
    EndDot = #vec{x=Endx,y=EndYf(Endx)},
    PpBeg = vec_len(vec_op('-',BegDot,SideDot1)),
    PpEnd = vec_len(vec_op('-',EndDot,SideDot1)),
    Vpnf = get_vpnf(Pv,Ppv,PovEnd),
    BegVpn = Vpnf(Beg),
    EndVpn = Vpnf(End),
    BotBeg = vec_len(BegVpn),
    BotEnd = vec_len(EndVpn),
    TopBeg = Height-BotBeg,
    TopEnd = Height-BotEnd,
    case
	(BotBeg < Hheight andalso BotEnd < Height andalso TopBeg > Hheight andalso TopEnd > Hheight)
	%% don't allow much distortion
	andalso abs(PpBeg-PpEnd) < vec_len(vec_op('-',Beg,End))
	%% case when point is inside lens
	%%andalso (dot_product(BegVpn,Pv) >= 0 andalso dot_product(EndVpn,Pv) >= 0)
	of
	true ->
	    %% first is sort key
	    {(TopBeg+TopEnd)/2,Color,[#vec{x=PpBeg,y=BotBeg},#vec{x=PpBeg,y=TopBeg},#vec{x=PpEnd,y=TopEnd},#vec{x=PpEnd,y=BotEnd}]};
	false ->
	    []
    end.

-spec faces(Begs,Color,Pns,SideDot1,Sides,Pov,Hfov,Heights) -> [] | {number(),Color,[#vec{}]} when Begs::#vec_pair{},
												   Color::{number(),number(),number()},
												   Pns::#vec_pair{},
												   SideDot1::#vec{},
												   Sides::#vec_pair{},
												   Pov::#vec_pair{},
												   Hfov::number(),
												   Heights::{number(),number()}.
faces(#vec_pair{left=Beg,right=End},Color,Pns = #vec_pair{left=Pn},SideDot1,#vec_pair{left=Side1,right=Side2},Pov = #vec_pair{left=PovBeg},Hfov,Heights) ->
    Begv = vec_op('-',Beg,PovBeg),
    Endv = vec_op('-',End,PovBeg),
    case Begv == #vec{x=0,y=0} orelse Endv == #vec{x=0,y=0} of
	true ->
	    [];
	false ->
	    case vec_angle(Begv,Pn) =< Hfov of
		true ->
		    case vec_angle(Endv,Pn) =< Hfov of
			true ->
			    %% whole face
			    gen_faces(#vec_pair{left=Beg,right=Begv},#vec_pair{left=End,right=Endv},Color,Pns,Pov,Heights,SideDot1);
			false ->
			    %% A + face part
			    gen_faces(#vec_pair{left=Beg,right=Begv},choose_intersect(Beg,End,PovBeg,Side1,Side2),Color,Pns,Pov,Heights,SideDot1)
		    end;
		false ->
		    case vec_angle(Endv,Pn) =< Hfov of
			true ->
			    %% B + face part
			    gen_faces(#vec_pair{left=End,right=Endv},choose_intersect(Beg,End,PovBeg,Side1,Side2),Color,Pns,Pov,Heights,SideDot1);
			false ->
			    Intersect1 = find_intersect(Beg,End,PovBeg,Side1),
			    case Intersect1 /= [] andalso vec_angle(vec_op('-',Intersect1,PovBeg),Pn) =< Hfov of
				true ->
				    Intersect2 = find_intersect(Beg,End,PovBeg,Side2),
				    case Intersect2 /= [] andalso vec_angle(vec_op('-',Intersect2,PovBeg),Pn) =< Hfov of
					true ->
					    %% face is bigger than fov
					    gen_faces(#vec_pair{left=Intersect1,right=Side1},#vec_pair{left=Intersect2,right=Side2},Color,Pns,Pov,Heights,SideDot1);
					false ->
					    %% face is not in fov
					    []
				    end;
				false ->
				    []
			    end
		    end
	    end
    end.

%%figures_handler(List,Tree,Fun,Procs) ->
%%    receive
%%	[] ->
%%	    figures_handler(List,Tree,Fun,Procs-1);
%%	[Key|Data] ->
%%	    figures_handler(List,enter([Key,0],Data,Tree),Fun,Procs-1);
%%	_ ->
%%	    figures_handler(List,Tree,Fun,Procs)
%%    after 0 ->
%%	    if Procs == 0 ->
%%		    Tree;
%%	       true ->
%%		    if List /= [] ->
%%			    [Head|Tail] = List,
%%			    spawn(fun() -> Fun(Head) end),
%%			    figures_handler(Tail,Tree,Fun,Procs);
%%		       true ->
%%			    figures_handler(List,Tree,Fun,Procs)
%%		    end
%%	    end
%%    end.

-spec plane_proj(Objs,Pns,SideDots,Sides,Pov,Hfov,Heights) -> [] | {number(),{number(),number(),number()},[#vec{}]} when Objs::[number()],
															 Pns::#vec_pair{},
															 SideDots::#vec_pair{},
															 Sides::#vec_pair{},
															 Pov::#vec_pair{},
															 Hfov::number(),
															 Heights::{number(),number()}.
plane_proj(Objs,Pns,#vec_pair{left=SideDot1},Sides,Pov,Hfov,Heights) ->
    lists:sort(lists:filter(fun(X) -> X /= [] end,
			    lists:map(fun([Line,Color]) -> faces(list_to_vec_pair(Line),Color,Pns,SideDot1,Sides,Pov,Hfov,Heights) end,Objs)))
    %% process faces in parallel
    %%figures_handler(Objs,gb_trees:empty(),fun([FaceBeg,FaceEnd]) ->
    %%						  faces(FaceBeg,FaceEnd,Pns,Tree,[Access,AccessPpn],Sides,Pov,Hfov,Heights,Parent) end,
    %%		    length(Objs))
	.

render_scene(Canvas,Objs,Pov,Pns,Sides,SideDots,Hfov,Heights) ->
    List = plane_proj(Objs,Pns,SideDots,Sides,Pov,Hfov,Heights),
    gl:clear(?GL_COLOR_BUFFER_BIT),
    lists:foreach(fun({_Key,{R,G,B},Dots}) ->
			  gl:'begin'(?GL_QUADS),
			  gl:color3ub(R,G,B),
			  lists:foreach(fun(#vec{x=X,y=Y}) -> gl:vertex2f(X,Y) end,Dots),
			  gl:'end'()
		  end,List),
    %%io:format("Drawn ~w faces~n",[(element(1,Tree))]),
    wxGLCanvas:swapBuffers(Canvas).
%%gl:getError = 0.

-spec event_handler(Canvas,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul) -> ok | no_return() when Canvas::{'wx_ref',integer(),_,_},
															 Steps::{number(),number()},
															 Frame::{'wx_ref',integer(),_,_},
															 Objs::[number()],
															 Pov::#vec_pair{},
															 Pns::#vec_pair{},
															 Sides::#vec_pair{},
															 SideDots::#vec_pair{},
															 Hfov::number(),
															 Hwidth::number(),
															 Heights::{number(),number()},
															 StepMul::#vec{}.
event_handler(Canvas,Steps = {Step,StepLow},Frame,Objs,Pov = #vec_pair{left=PovBeg,right=PovEnd},Pns = #vec_pair{right=Ppn},Sides,SideDots,Hfov,Hwidth,Heights,StepMul) ->
    receive
	{wx,_,_,_,{wxKey,key_up,_,_,Key,_,_,_,_,_,_,_,_}} ->
	    case Key of
		%% forward or backward
		_ when Key == 87 orelse Key == 83 ->
		    Inc = if Key == 87 ->
				  '+';
			     true ->
				  '-'
			  end,
		    PovEndNew = vec_op(Inc,PovEnd,StepMul),
		    PovNew = #vec_pair{left=vec_op(Inc,PovBeg,StepMul),right=PovEndNew},
		    SideDotsNew = get_side_dots(PovEndNew,Ppn,Hwidth),
		    wxFrame:setStatusText(Frame,io_lib:format("POV: ~p", [PovNew])),
		    render_scene(Canvas,Objs,PovNew,Pns,Sides,SideDotsNew,Hfov,Heights),
		    event_handler(Canvas,Steps,Frame,Objs,PovNew,Pns,Sides,SideDotsNew,Hfov,Hwidth,Heights,StepMul);
		%% turn around
		_ when Key == 65 orelse Key == 68 ->
		    PovNew = if Key == 65 ->
				     #vec_pair{left=PovBeg,right=vec_rot(PovEnd,PovBeg,StepLow)};
				true ->
				     #vec_pair{left=PovBeg,right=vec_rot(PovEnd,PovBeg,-StepLow)}
			     end,
		    {PnsNew = #vec_pair{left=PvNew},SideDotsNew,SidesNew} = recalc_plane(PovNew,Hwidth),
		    StepMulNew = vec_scal_mul(vec_norm(PvNew),Step),
		    wxFrame:setStatusText(Frame,io_lib:format("POV: ~p", [PovNew])),
		    render_scene(Canvas,Objs,PovNew,PnsNew,SidesNew,SideDotsNew,Hfov,Heights),
		    event_handler(Canvas,Steps,Frame,Objs,PovNew,PnsNew,SidesNew,SideDotsNew,Hfov,Hwidth,Heights,StepMulNew);
		_ ->
		    event_handler(Canvas,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul)
	    end;
	%%{wx,_,_,_,{wxSize,size,{Width,Height},_}} ->
	%%    HwidthNew = Width/2,
	%%    SideDotsNew = get_side_dots(PovEnd,Ppn,HwidthNew),
	%%    SidesNew = [Side1New,_] = get_sides(PovBeg,SideDotsNew),
	%%    HfovNew = vec_angle(vec_norm(Side1New),Pn),
	%%    HeightsNew = [Height,Height/2],
	%%    gl:loadIdentity(),
	%%    gl:viewport(0,0,Width,Height),
	%%    glu:ortho2D(0,Width,0,Height),
	%%    render_scene(Canvas,Objs,Pov,Pns,SidesNew,SideDotsNew,HfovNew,HeightsNew,Parent),
	%%    event_handler(Canvas,Parent,Steps,Frame,Objs,Pov,Pns,SidesNew,SideDotsNew,HfovNew,HwidthNew,HeightsNew,StepMul);
	{wx,_,_,_,{wxClose,close_window}} ->
	    ok;
	{wx,10,_,_,_} ->
	    wxWindow:close(Frame),
	    ok;
	%%{'_wxe_error_',_,_} ->
	%%    fail;
	{ngranek,Cmd} ->
	    case Cmd of
		pov ->
		    io:format("You're ~p~n",[Pov]);
		objs ->
		    io:format("~p~n",[Objs]);
		_ ->
		    []
	    end,
	    event_handler(Canvas,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul);
	%%{ngranek,pov,PovNew} ->
	%%    case PovNew of
	%%	[?ZERO,?ZERO] ->
	%%	    event_handler(Canvas,Parent,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul);
	%%	[[X1,Y1],[X2,Y2]] when is_number(X1) andalso is_number(Y1) andalso is_number(X2) andalso is_number(Y2) ->
	%%	    [PnsNew = [PnNew,_],SideDotsNew,SidesNew] = recalc_plane(PovNew,Hwidth),
	%%	    StepMulNew = vec_scal_mul(PnNew,Step),
	%%	    wxFrame:setStatusText(Frame,io_lib:format("POV: ~p", [PovNew])),
	%%	    render_scene(Canvas,Objs,PovNew,PnsNew,SidesNew,SideDotsNew,Hfov,Heights,Parent),
	%%	    event_handler(Canvas,Parent,Steps,Frame,Objs,PovNew,PnsNew,SidesNew,SideDotsNew,Hfov,Hwidth,Heights,StepMulNew);
	%%	_ ->
	%%	    event_handler(Canvas,Parent,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul)
	%%    end;
	%%{ngranek,objs,ObjsNew} ->
	%%    case ObjsNew of
	%%	_ when is_list(ObjsNew) ->
	%%	    Test = lists:all(fun(Elm) -> case Elm of 
	%%					     [Beg = [X1,Y1],End = [X2,Y2],[R,G,B]] when is_number(X1) andalso is_number(Y1)
	%%											andalso is_number(X2) andalso is_number(Y2)
	%%											andalso is_integer(R) andalso is_integer(G)
	%%											andalso is_integer(B) ->
	%%						 vec_op(fun(C,D) -> C-D end,End,Beg) /= ?ZERO;
	%%					     _ ->
	%%						 false
	%%					 end end,ObjsNew),
	%%	    if Test ->
	%%		    render_scene(Canvas,ObjsNew,Pov,Pns,Sides,SideDots,Hfov,Heights,Parent),
	%%		    event_handler(Canvas,Parent,Steps,Frame,ObjsNew,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul);
	%%	       true ->
	%%		    event_handler(Canvas,Parent,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul)
	%%	    end;
	%%	_ ->
	%%	    event_handler(Canvas,Parent,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul)
	%%    end;
	_Event ->
	    %%io:format("Received ~p~n",[_Event]),
	    event_handler(Canvas,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul)
    after 200 ->	    
	    event_handler(Canvas,Steps,Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul)
    end.

-spec start() -> no_return().
start() ->
    Data = #data{},
    [Width,Height_] = Data#data.screen,
    Frame = wxFrame:new(wx:new(),1,"ngranek raycaster",[{size,{Width,Height_}}]),
    Bar = wxMenuBar:new(),
    _Statbar = wxFrame:createStatusBar(Frame,[{number,1}]),
    Height = element(2,wxWindow:getClientSize(Frame)),
    Menu = wxMenu:new(),
    Canvas = wxGLCanvas:new(Frame),
    Pov = list_to_vec_pair(Data#data.pov),
    Step = Data#data.step,
    Heights = {Height,Height/2},
    Hwidth = Width/2,
    {Pns = #vec_pair{left=Pv},SideDots,Sides = #vec_pair{left=Side1}} = recalc_plane(Pov,Hwidth),
    Hfov = vec_angle(Side1,Pv),
    StepMul = vec_scal_mul(vec_norm(Pv),Step),
    %% assign colors
    Objs = lists:flatmap(fun([Color,Faces]) -> lists:map(fun(Face) -> [Face,Color] end,Faces) end,Data#data.objs),
    wxFrame:connect(Frame,close_window),
    wxFrame:connect(Frame,command_menu_selected),
    wxFrame:setMenuBar(Frame,Bar),
    wxMenuBar:append(Bar,Menu,"&File"),
    wxMenu:append(Menu,10,"Exit"),
    wxWindow:show(Frame),
    wxWindow:connect(Canvas,size),
    wxWindow:connect(Canvas,key_up),
    wxGLCanvas:setCurrent(Canvas),
    gl:viewport(0,0,Width,Height),
    glu:ortho2D(0,Width,0,Height),
    wxFrame:setStatusText(Frame,io_lib:format("POV: ~p",[Pov])),
    render_scene(Canvas,Objs,Pov,Pns,Sides,SideDots,Hfov,Heights),
    event_handler(Canvas,{Step,Step*0.01},Frame,Objs,Pov,Pns,Sides,SideDots,Hfov,Hwidth,Heights,StepMul).
%%wx:destroy().
