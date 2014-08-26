%% auther: xyao   
%% email: jiexiaowen@gmail.com   
%% date: 2010.06.12
  
{   
    application, gs,
    [   
        {description, "This is a game server."},   
        {vsn, "1.0a"},   
        {modules,[gs]},
        {registered, [gs_app]},
        {applications, [kernel, stdlib, sasl]},   
        {mod, {gs_app, []}},
        {start_phases, []},
		{env,[ 
                {server, "T1"}
           ]
        }
    ]   
}.    
 
%% File end.  
