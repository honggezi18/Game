%% auther: 严利宏   
%% email: 542430172@qq.com   
%% date: 2014.08.27
  
{   
    application, yx,
    [   
        {description, "This is a youxi game server."},   
        {vsn, "1.0"},   
        {modules,[yx]},
        {registered, [yx_app]},
        {applications, [kernel, stdlib, sasl]},   
        {mod, {yx_app, []}},
        {start_phases, []},
		{env,[ 
                {server, "T1"}
           ]
        }
    ]   
}.    
 
%% File end.  
