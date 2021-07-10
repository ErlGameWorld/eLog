概述
--------
eLog is a Erlang logger. 基于lager3.9.0 rewrite

TODO
--------
trace相关代码整理

特点
--------

* 支持各种日志级别(debug、info、notice、warning、error、critical、alert、emergency)
* 使用解析转换进行转换，以允许捕获 Module/Function/Line/Pid 等信息
* 当没有处理程序正在使用日志级别时(例如。没有事件被发送到日志处理器
* 支持多个后端(bkds), 包括控制台和文件
* 支持多个接收器(sinks)
* 将常见的OTP错误消息改写为更易读的消息
* 在面对较大或较多的日志消息时，不会出现节点内存不足的情况
* 绕过日志大小截断的可选特性("unsafe")
* 支持内部时间和日期旋转，以及外部旋转工具
* Syslog style日志级别比较标志
* 彩色的终端输出
* 可选负载削减设置高水位线杀死(并重新安装)后，可配置冷却定时器的接收器(sinks)
* 重写日志的format函数，这样更有效率

接受器(Sinks)
-----
eLog传统上支持名为`eLogEmm`的单一接收器(sink)(实现为`gen_emm`管理器)，所有后端都连接到该接收器。 eLog现在支持额外的接收器(sink);每个接收器(sink)可以有不同的 sync/async
消息阈值和不同的后端。

### 接收器的配置

要使用多个接收器（超出eLog和eLog_event的内置接收器），您需要：

1. 设置rebar.config
2. 在app.config中配置后端

#### Runtime requirements

```erlang
[{eLog, [
   {logRoot, "/tmp"},
   %% Default handlers for eLog
   {handlers, [
      {lgBkdConsole, [{level, info}]},
      {lgBkdFile, [{file, "error.log"}, {level, error}]},
      {lgBkdFile, [{file, "console.log"}, {level, info}]}
   ]},
   
   %% Any other sinks
   {extraSinks, [
      {auditLgEvent, [{handlers, [{lgBkdFile, [{file, "sink1.log"}, {level, info}]}]}, {asyncThreshold, 500}, {asyncThrWindow, 50}]}
   ]}
]}].
```

自定义格式
-----------------
All loggers have a default formatting that can be overriden. A fmtTer is any module that
exports `format(#lgMsg{}, Config#any())`. It is specified as part of the configuration for the backend:

```erlang
{eLog, [
   {handlers, [
      {lgBkdConsole, [{level, info}, {fmtTer, lgFmtTer},{fmtCfg, [time, " [",severity, "] ", message, "\n"]}]},
      {lgBkdFile, [{file, "error.log"}, {level, error}, {fmtTer, lgFmtTer}, {fmtCfg, [date, " ", time, " [", severity, "] ",pid, " ", message, "\n"]}]},
      {lgBkdFile, [{file, "console.log"}, {level, info}]}
   ]}
]}.
```

包括lgFmtTer。这使用类似于Erlang的iolist的结构（称为“ semi-iolist”）为日志消息提供了一种通用的默认格式 ：

配置中的任何传统iolist元素均按原样打印。 配置中的原子被视为较大元数据的占位符，并从日志消息中提取。 占位符date，time，message，sev并severity会一直存在。
sev是缩写的严重性，它被解释为严重性级别的大写单字母编码（例如'debug'-> $D） 占位符pid，file，line，module，function，和node 永远如果使用解析变换存在。
application如果使用解析转换，则占位符可能存在。它取决于查找应用程序app.src文件。 如果使用错误记录器集成，则占位符pid 将始终存在并且占位符name可能存在。 应用程序可以定义自己的元数据占位符。 元组{atom(),
semi-iolist()}允许原子占位符的后备。如果找不到由原子表示的值，则将解释半iolist。 一个元组{atom(), semi-iolist(), semi-iolist()
}代表一个条件运算符：如果可以找到原子占位符的值，则将输出第一个半iolist。否则，将使用第二个。 元组{pterm, atom()}将尝试从 OTP 21.2中添加的persistent_term功能中查找指定原子的值 。默认值为""
。如果找不到密钥或在OTP 21之前的OTP版本中指定了此格式术语，则将使用默认值。 元组{pterm, atom(), semi-iolist()}将尝试从OTP
21.2中添加的persistent_term功能中查找指定原子的值。默认值为指定的semi-iolist（）。如果找不到密钥，或者在OTP 21之前的OTP版本中指定了此格式术语，则将使用默认值。

Examples:

```
["Foo"] -> "Foo", regardless of message content.
[message] -> The content of the logged message, alone.
[{pid,"Unknown Pid"}] -> "<?.?.?>" if pid is in the metadata, "Unknown Pid" if not.
[{pid, ["My pid is ", pid], ["Unknown Pid"]}] -> if pid is in the metadata print "My pid is <?.?.?>", otherwise print "Unknown Pid"
[{server,{pid, ["(", pid, ")"], ["(Unknown Server)"]}}] -> user provided server metadata, otherwise "(<?.?.?>)", otherwise "(Unknown Server)"
[{pterm, pterm_key, <<"undef">>}] -> if a value for 'pterm_key' is found in OTP 21 (or later) persistent_term storage it is used, otherwise "undefined"
```

错误记录器集成
------------------------
error_logger eLog还提供了一个处理程序模块，该模块将传统的erlang错误消息转换为更友好的格式，并将其发送到贮藏啤酒中，以像常规贮藏啤酒日志调用一样对待。
要禁用此功能，请将更大的应用程序变量设置errLoggerRedirect为false。您也可以通过设置变量OTP和牛仔消息禁用重新格式化 errLoggerFmtRaw 为 true。

如果您将自己的处理程序安装到中error_logger，则可以通过使用errLoggerWhitelist环境变量和允许的处理程序列表来告诉eLog使其不被处理。

```
{errLoggerWhitelist, [my_handler]}
```

该error_logger处理器也将记录较完整的错误信息（使用的保护trunc_io）到“崩溃日志”，它可以被称为进一步的信息。崩溃日志的位置可以由crash_log 应用程序变量指定。如果设置为false完全不写入。

崩溃日志中的消息受最大消息大小的限制，可以通过crashLogMsgSize应用程序变量指定最大消息大小。

如果已定义来自的消息，error_logger则将其重定向到接收器lgErrLoggerH，以便可以将其重定向到另一个日志文件。

例如：

```
[{eLog, [
         {extraSinks,
          [
            {error_logger_event, 
                [{handlers, [
                    {lgBkdFile, [{file, "error_logger.log"}, {level, info}]}]}]
            }]
           }]
}].
```

will send all `error_logger` messages to `error_logger.log` file.

过载保护
-------------------

### 异步模式

在eLog 2.0之前，eLog的核心纯粹是在同步模式下运行。异步模式速度更快，但是无法防止消息队列过载。从更大的2.0开始，gen_event采用了一种混合方法。 它轮询自己的邮箱大小，并根据邮箱大小在同步和异步之间切换消息传递。

```erlang
{asyncThreshold, 20},
{asyncThrWindow, 5}
```

这将使用异步消息传递，直到邮箱超过20条消息为止，此时将使用同步消息传递，并在大小减小为时切换回异步20 - 5 = 15。
如果您想禁用此行为，只需设置async_threshold为undefined。它默认为一个较小的数字，以防止邮箱迅速增长超过限制并引起问题。通常，较大的消息应尽可能快地处理它们，因此无论如何落后20条消息都是相对异常的。
如果要限制每秒允许的消息数error_logger，如果要在许多相关进程崩溃时应对大量消息，这是一个好主意，则可以设置一个限制：

```erlang
{errLoggerHwm, 50}
```

最好将此数字保持较小。

### 事件队列刷新

超过高水位标记时，可以将啤酒配置为刷新消息队列中的所有事件通知。这可能会对同一事件管理器（例如中的error_logger）中的其他处理程序产生意想不到的后果，因为它们所依赖的事件可能会被错误地丢弃。默认情况下，此行为已启用，但可以通过以下方式进行控制error_logger：

```erlang
{errLoggerFlushQueue, true | false}
```

or for a specific sink, using the option:

```erlang
{flushQueue, true | false}
```

如果flushQueue为true，则可以设置消息队列长度阈值，在该阈值处将开始丢弃消息。默认阈值为0，这意味着如果flushQueue为true，则超过高水位标记时将丢弃消息，而不管消息队列的长度如何。用于控制阈值的选项是error_logger：

```erlang
{errLoggerFlushThr, 1000}
```

and for sinks:

```erlang
{flushThreshold, 1000}
```

### 接收器(Sink) Killer

在某些高容量情况下，最好丢弃所有待处理的日志消息，而不是让它们随着时间流失。

如果您愿意，可以选择使用水槽杀手来减轻负载。在此操作模式下，如果gen_event邮箱超过可配置的高水位线，则在可配置的冷却时间后，水槽将被杀死并重新安装。 您可以使用以下配置指令来配置此行为：

```erlang
{killerHwm, 1000},
{killerReTime, 5000}
```

这意味着，如果接收器的邮箱大小超过1000条消息，请杀死整个接收器并在5000毫秒后重新加载它。如果需要，此行为也可以安装到其他水槽中。
默认情况下，管理器杀手未安装到任何接收器中。如果killerReTime未指定冷却时间，则默认为5000。

"不安全"
--------
不安全的代码路径会绕过普通的格式代码，并使用与OTP中的error_logger相同的代码。这可以稍微提高您的日志记录代码的速度（在基准测试期间，我们测得的改进幅度为0.5-1.3％；其他报告的改进则更好。）
这是一个危险的功能。它不会保护您免受大型日志消息的侵害-大型消息可能会杀死您的应用程序，甚至由于内存耗尽而导致Erlang VM死机，因为在故障级联中反复复制大量术语。我们强烈建议此代码路径仅用于上限大小约为500字节的有边界的日志消息。
如果日志消息有可能超过该限制，则应使用常规的更大的消息格式化代码，该代码将提供适当的大小限制并防止内存耗尽。 如果要格式化不安全的日志消息，则可以使用严重性级别（照常），然后使用_unsafe。这是一个例子：


运行时日志级别更改
------------------------
您可以通过执行以下操作在运行时更改任何大型后端的日志级别：

```erlang
eLog:setLoglevel(lgBkdFileConsole, debug).
```

Or, for the backend with multiple handles (files, mainly):

```erlang
eLog:setLoglevel(lgBkdFile, "console.log", debug).
```

eLog会跟踪任何后端使用的最低日志级别，并禁止生成低于该级别的消息。这意味着当没有后端使用调试消息时，调试日志消息实际上是免费的。一个简单的基准测试，它可以在不超过最小阈值的情况下完成一百万条调试日志消息，而所需时间不到半秒。

Syslog样式日志级别比较标志
--------------------------------------
除了常规的日志级别名称，您还可以对要记录的内容进行更细粒度的屏蔽：

```
info - info and higher (>= is implicit)
=debug - only the debug level
!=info - everything but the info level
<=notice - notice and below
<warning - anything less than warning
```

它们可以在提供日志级别的任何地方使用，尽管它们必须是带引号的原子或字符串。

内部日志旋转
---------------------
啤酒可以轮换自己的日志，也可以通过外部流程完成日志。要使用内旋，使用size，date并count在文件后端的配置值：

```erlang
[{file, "error.log"}, {level, error}, {size, 10485760}, {date, "$D0"}, {count, 5}]
```

这告诉啤酒将错误和以上消息记录到error.log文件，并在午夜或到达10mb时旋转文件（以先到者为准），并在当前文件之外保留5个旋转的日志。将count设置为0不会禁用旋转，而是旋转文件，并且不保留以前的版本。要禁用旋转，请将大小设置为0，将日期设置为“”。

该$D0语法来自newsyslog.conf中newsyslog使用的语法。相关摘录如下：

```
Hour, Day, week and month 时间格式:
以`$'符号作为时间格式的前缀
hour、day、week、month规范的具体格式分别为:[Hmm] [Dhh]、[WwDhh]和[MddDhhHmm]
可选时间字段默认为midnightthe start of the day)
The ranges for hour, day and hour specifications are:
  mm      minutes, range 0 ... 59
  hh      hours, range 0 ... 23
  w       day of week, range 1 ... 7
  dd      day of month, range 1 ... 31, or the letter L or l to specify the last day of the month.

Some examples:
  $D0     rotate every night at midnight
  $D23    rotate every day at 23:00 hr
  $W0D23  rotate every week on Sunday at 23:00 hr
  $W5D16  rotate every week on Friday at 16:00 hr
  $M1D0   rotate on the first day of every month at midnight (i.e., the start of the day)
  $M5D6   rotate on every 5th day of the month at 6:00 hr
  $H00    rotate every hour at HH:00
  $D12H30 rotate every day at 12:30
  $W0D0H0 rotate every week on Sunday at 00:00
```

To configure the crash log rotation, the following application variables are used:

* `crashLogFileSize`
* `crashLogDate`
* `crashLogCount`
* `crashLogRotator`

See the `.app.src` file for further details.

自定义日志轮换
-------------------

```erlang
{rotator, lgRotatorIns}
```

The module should provide the following callbacks as `lgRotatorExm`


彩色端子输出
-----------------------
If you have Erlang R16 or higher, you can tell eLog's console backend to be colored. Simply add to eLog's application
environment config:

```erlang
{colored, true}
```

If you don't like the default colors, they are also configurable; see the `.app.src` file for more details.

The output will be colored from the first occurrence of the atom color in the formatting configuration. For example:

```erlang
{lgBkdConsole, [{level, info}, {fmtTer, lgFmtTer},
{fmtCfg, [time, color, " [", severity, "] ", message, "\e[0m\r\n"]}]]}
```

This will make the entire log message, except time, colored. The escape sequence before the line break is needed in
order to reset the color after each log message.

Tracing
-------
eLog支持基于日志消息属性重定向日志消息的基本支持。啤酒会自动在日志消息呼叫站点捕获pid，模块，功能和行。但是，您可以添加所需的任何其他属性：

```erlang
eLog:warning([{request, RequestID}, {vhost, Vhost}], "Permission denied to ~s", [User])
```

然后，除了默认的跟踪属性外，您还可以基于请求或虚拟主机进行跟踪：

```erlang
eLog:trace_file("logs/example.com.error", [{vhost, "example.com"}], error)
```

要在过程的整个生命周期中保留元数据，可以使用eLog:md/1将元数据存储在过程字典中：

```erlang
eLog:md([{zone, forbidden}])
```

请注意，eLog:md将仅接受由原子键控的键/值对的列表。 您也可以省略最后一个参数，日志级别默认为 debug。 跟踪到控制台是类似的：

```erlang
eLog:trace_console([{request, 117}])
```

在上面的示例中，省略了日志级别，但是如果需要，可以将其指定为第二个参数。 您还可以在过滤器中指定多个表达式，或将*原子用作通配符以匹配具有该属性的任何消息，而不管其值如何。您也可以使用特殊值!来表示，仅在不存在此键的情况下才选择。
还支持跟踪到现有日志文件（但请参阅下面的“多接收器支持”）：

```erlang
eLog:trace_file("log/error.log", [{module, mymodule}, {function, myfunction}], warning)
```

要查看活动的日志后端和跟踪，可以使用该eLog:status() 功能。要清除所有活动迹线，可以使用eLog:clear_all_traces()。

要删除特定跟踪，请在创建跟踪时存储该跟踪的句柄，然后将其传递给eLog:stop_trace/1： to `eLog:stop_trace/1`:

```erlang
{ok, Trace} = eLog:trace_file("log/error.log", [{module, mymodule}]),
...
eLog:stop_trace(Trace)
```

跟踪到pid有点特殊情况，因为pid并不是序列化良好的数据类型。要按pid进行跟踪，请使用pid作为字符串：

```erlang
eLog:trace_console([{pid, "<0.410.0>"}])
```

### 过滤表达式

从更大的3.3.1版本开始，您还可以在跟踪第二个元素是比较运算符的地方使用3元组。当前支持的比较运算符为：

* `<` - less than
* `=<` - less than or equal
* `=` - equal to
* `!=` - not equal to
* `>` - greater than
* `>=` - greater than or equal

```erlang
eLog:trace_console([{request, '>', 117}, {request, '<', 120}])
```

Using `=` is equivalent to the 2-tuple form.

### Filter composition

作为啤酒3.3.1，你也可以使用的特殊滤光器构成键 all或any。例如，上面的过滤器示例可以表示为：

```erlang
eLog:trace_console([{all, [{request, '>', 117}, {request, '<', 120}]}])
```

any在过滤器之间具有“或样式”逻辑评估的效果；all 表示过滤器之间的“ AND样式”逻辑评估。这些合成过滤器期望附加过滤器表达式的列表作为它们的值。

### 空过滤器

该null滤波器具有特殊的意义。过滤器{null, false}充当黑洞；什么都没有通过。筛选器{null, true}意味着 一切都会通过。null过滤器的其他任何值均无效，将被拒绝。

### 支持多个接收器

如果使用多个接收器，则应注意跟踪的限制。 跟踪特定于接收器，可以通过跟踪过滤器进行指定：

```erlang
eLog:trace_file("log/security.log", [{sink, audit_event}, {function, myfunction}], warning)
```

如果未指定接收器，则将使用默认的更大接收器。

这有两个后果：

跟踪无法拦截发送到其他接收器的消息。 eLog:trace_file仅当指定了相同的接收器时，才能跟踪到已经通过打开的文件。 可以通过打开多条迹线来改善前者。后者可以通过重新配置更大的文件后端来解决，但尚未解决。

### 配置跟踪

eLog支持从其配置文件启动跟踪。定义它们的关键字是traces，其后是一个元组属性列表，该元组定义了后端处理程序和必需列表中的零个或多个过滤器，后跟一个可选的消息严重性级别。

一个例子看起来像这样：

```erlang
{eLog, [
   {handlers, [...]},
      {traces, [
       %% handler,                         filter,                message level (defaults to debug if not given)
         {lgBkdConsole, [{module, foo}], info},
         {{lgBkdFile, "trace.log"}, [{request, '>', 120}], error},
         {{lgBkdFile, "event.log"}, [{module, bar}]} %% implied debug level here
      ]}
]}.
```

在这个例子中，我们有三个痕迹。一种使用控制台后端，另外两种使用文件后端。如果忽略了消息严重性级别，则默认debug为上一个文件后端示例中的级别。

该traces关键字也适用于其他接收器，但适用上述相同的限制和警告。

重要信息：您必须在3.1.0（含）以下的所有更大版本中定义严重性级别。直到3.2.0才添加2元组形式。

在编译时设置动态元数据
----------------------------------------
eLog支持通过注册回调函数从外部源提供元数据。即使进程死亡，该元数据也将在整个进程中保持不变。

通常，您不需要使用此功能。但是，它在以下情况下很有用：

seq_trace提供的跟踪信息 有关您的应用程序的上下文信息 默认占位符未提供的持久性信息

on_emit：

直到后端发出消息后，回调函数才能解析。 如果回调函数无法解析，未加载或产生未处理的错误，undefined则将返回该函数。 由于回调函数依赖于进程，因此有可能在依赖进程死亡导致undefined返回后发出消息。这个过程也可以是你自己的过程
on_log：

无论是否 发出消息，都将解析回调函数 如果无法解析或未加载回调函数，则啤酒本身不会处理错误。 回调中的任何潜在错误都应在回调函数本身中处理。
由于该函数在日志记录时已解析，因此在解决该依赖进程之前，依赖进程死机的可能性应该较小，尤其是如果您正在从包含回调的应用程序进行日志记录时。 第三个元素是函数的回调，该函数由形式的元组组成{Module
Function}。无论使用on_emit还是，回调都应如下所示on_log：

应该将其导出 它不应该接受任何参数，例如arity为0 它应该返回任何传统的iolist元素或原子 undefined 有关在回调中生成的错误，请参见上面的解析类型文档。
如果回调返回，undefined则它将遵循与“自定义格式”部分中记录的相同的后备和条件运算符规则 。

此示例可以使用，on_emit但与一起使用可能是不安全的 on_log。如果呼叫失败，on_emit则默认为undefined，但是on_log会出错。

```erlang
-export([my_callback/0]).

my_callback() ->
   my_app_serv:call('some options').
```

This example would be to safe to work with both `on_emit` and `on_log`

```erlang
-export([my_callback/0]).

my_callback() ->
   try my_app_serv:call('some options') of
      Result ->
         Result
   catch
      _ ->
         %% You could define any traditional iolist elements you wanted here
         undefined
   end.
```

注意，回调可以是任何Module：Function / 0。它不是您的应用程序的一部分。例如，您可以 像这样将其cpu_sup:avg1/0用作 回调函数{cpu_avg1, on_emit, {cpu_sup, avg1}}

例子：

```erlang
-export([reductions/0]).

reductions() ->
   proplists:get_value(reductions, erlang:process_info(self())).
```

```erlang
-export([seq_trace/0]).

seq_trace() ->
   case seq_trace:get_token(label) of
      {label, TraceLabel} ->
         TraceLabel;
      _ ->
         undefined
   end.
```

重要信息：由于on_emit依赖于发出日志消息时注入的函数调用，因此您的日志记录性能（操作数/秒）将受到调用函数的作用以及它们可能引入多少延迟的影响。on_log由于呼叫是在记录消息的时刻注入的，因此这种影响将更大。

在编译时设置截断限制
--------------------------------------------
eLog默认将消息截断为4096字节，您可以使用该{eLog_truncation_size, X}选项进行更改。在钢筋中，您可以将其添加到 erl_opts：

禁止应用程序和主管启动/停止日志
-----------------------------------------------------
如果您不想在应用程序的调试级别看到主管和应用程序启动/停止日志，则可以使用以下配置将其关闭：

```erlang
{eLog, [{suppressAppStartStop, true},
{suppressSupStartStop, true}]}
```

系统调试功能
--------------------
贮藏啤酒提供了一种使用sys“调试功能”的集成方式。您可以通过执行以下操作在目标进程中安装调试功能：

```erlang
eLog:install_trace(Pid, notice).
```

You can also customize the tracing somewhat:

```erlang
eLog:install_trace(Pid, notice, [{count, 100}, {timeout, 5000}, {format_string, "my trace event ~p ~p"]}).
```

跟踪选项当前为：

超时-跟踪保持安装的时间infinity：（默认）或毫秒超时 count-要记录的跟踪事件数：（infinity默认）或正数 format_string-用于记录事件的格式字符串。对于提供的2个参数，必须具有2个格式说明符。
这将在OTP进程的每个“系统事件”上（通常是入站消息，答复和状态更改）在指定的日志级别生成一个更大的消息。

完成以下操作后，您可以删除跟踪：

```erlang
eLog:remove_trace(Pid).
```

If you want to start an OTP process with tracing enabled from the very beginning, you can do something like this:

```erlang
gen_server:start_link(mymodule, [], [{debug, [{install, {fun eLog:trace_func/3, eLog:trace_state(undefined, notice, [])}}]}]).
```

trace_state函数的第三个参数是上面记录的“选项”列表。

控制台输出到另一个组长进程
----------------------------------------------

如果要将控制台输出发送到另一个group_leader（通常在另一个节点上），则可以为{group_leader, Pid}控制台后端提供一个参数。 可以将其与另一个控制台配置选项id和gen_event结合使用，{Module,
ID}以允许通过nodetool远程跟踪节点以进行标准输出：

```erlang
GL = erlang:group_leader(),
Node = node(GL),
eLog_app:start_handler(eLog_event, {eLog_console_backend, Node},
[{group_leader, GL}, {level, none}, {id, {eLog_console_backend, Node}}]),
case eLog:trace({eLog_console_backend, Node}, Filter, Level) of
...
```

在上面的示例中，假定代码正在通过nodetool rpc 调用运行，以便代码在Erlang节点上执行，但是group_leader是reltool节点的代码（例如appname_maint_12345@127.0.0.1）。
如果打算将此功能与跟踪配合使用，请确保start_handler的第二个参数与该id参数匹配。因此，当自定义group_leader进程退出时，贮藏啤酒将删除该处理程序的所有关联跟踪。
