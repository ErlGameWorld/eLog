-module(lgRotatorExm).

%% Create a log file
-callback createLogFile(Name :: list(), Buffer :: {integer(), integer()} | any()) ->
   {ok, Fd :: file:io_device(), Inode :: integer(), DateTime :: file:date_time(), FileSize :: integer()} |
   {error, any()}.

%% Open a log file
-callback openLogFile(Name :: list(), Buffer :: {integer(), integer()} | any()) ->
   {ok, Fd :: file:io_device(), Inode :: integer(), DateTime :: file:date_time(), FileSize :: integer()} |
   {error, any()}.

%% Ensure reference to current target, could be rotated
-callback ensureLogFile(Name :: list(), FD :: file:io_device(), Inode :: integer(), CTime :: file:date_time(), Buffer :: {integer(), integer()} | any()) ->
   {ok, Fd :: file:io_device(), Inode :: integer(), DateTime :: file:date_time(), FileSize :: integer()} |
   {error, any()}.

%% Rotate the log file
-callback rotateLogFile(Name :: list(), Count :: integer()) ->
   ok.
