%% User's properties.
%% Each user belongs to General group on the beginning.
-record(user, {name = "Anonymous",
               password = "",
               socket, 
               mode = not_connected,
               groups = [],
               node = localnode
              }).
