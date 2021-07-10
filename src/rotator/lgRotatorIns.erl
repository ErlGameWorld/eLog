-module(lgRotatorIns).

-behaviour(lgRotatorExm).

-include_lib("kernel/include/file.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   createLogFile/2
   , openLogFile/2
   , ensureLogFile/5
   , rotateLogFile/2
]).

createLogFile(FileName, Buffer) ->
   openLogFile(FileName, Buffer).

openLogFile(FileName, Buffer) ->
   case filelib:ensure_dir(FileName) of
      ok ->
         Options =
            case Buffer of
               {Size, Interval} ->
                  [append, raw, {delayed_write, Size, Interval}];
               _ ->
                  [append, raw]
            end,
         case file:open(FileName, Options) of
            {ok, Fd} ->
               case file:read_file_info(FileName, [raw]) of
                  {ok, FileInfo} ->
                     #file_info{size = FileSize, inode = Inode} = FileInfo,
                     CTime = tryUpdateCTime(FileName, FileInfo),
                     {ok, Fd, Inode, CTime, FileSize};
                  RfiErr -> RfiErr
               end;
            OpErr -> OpErr
         end;
      EnDirErr -> EnDirErr
   end.

ensureLogFile(FileName, Fd, Inode, CTime, Buffer) ->
   case Fd of
      undefined ->
         openLogFile(FileName, Buffer);
      _ ->
         case lgUtil:isFileChanged(FileName, Inode, CTime) of
            {true, _FInfo} ->
               reopenLogFile(FileName, Fd, Buffer);
            {_, FInfo} ->
               {ok, Fd, Inode, CTime, FInfo#file_info.size}
         end
   end.

reopenLogFile(FileName, Fd, Buffer) ->
   %% Flush and close any file handles. delayed write can cause file:close not to do a close
   _ = file:datasync(Fd),
   _ = file:close(Fd),
   _ = file:close(Fd),
   openLogFile(FileName, Buffer).

rotateLogFile(_File, _Count) ->
   ok.

%% %% renames failing are OK
%% rotateLogFile(File, 0) ->
%%    %% open the file in write-only mode to truncate/create it
%%    case file:open(File, [write]) of
%%       {ok, FD} ->
%%          _ = file:close(FD),
%%          _ = file:close(FD),
%%          tryUpdateCTime(File),
%%          ok;
%%       Error ->
%%          Error
%%    end;
%% rotateLogFile(File0, 1) ->
%%    File1 = <<File0/binary, ".0">>,
%%    _ = file:rename(File0, File1),
%%    rotateLogFile(File0, 0);
%% rotateLogFile(File0, Count) ->
%%    File1 = <<File0/binary, ".", (integer_to_binary(Count - 2))/binary>>,
%%    File2 = <<File0/binary, ".", (integer_to_binary(Count - 1))/binary>>,
%%    _ = file:rename(File1, File2),
%%    rotateLogFile(File0, Count - 1).
%%
%% tryUpdateCTime(Name) ->
%%    case file:read_file_info(Name, [raw]) of
%%       {ok, FInfo} ->
%%          tryUpdateCTime(Name, FInfo);
%%       _ ->
%%          erlang:localtime()
%%    end.

tryUpdateCTime(Name, FileInfo) ->
   case os:type() of
      {win32, _} ->
         %注意：我们将创建时间强制为当前时间。在win32上，这可能会阻止ctime的更新：https://stackoverflow.com/q/8804342/1466825
         NewCtime = erlang:localtime(),
         NewFileInfo = FileInfo#file_info{ctime = NewCtime},
         ok = file:write_file_info(Name, NewFileInfo, [raw]),
         NewCtime;
      _ ->
         element(#file_info.ctime, FileInfo)
   end.
