https://www.reddit.com/r/erlang/comments/7jfqw9/deleted_by_user/
https://medium.com/@jlouis666/an-erlang-otp-20-0-optimization-efde8b20cba7
https://news.ycombinator.com/item?id=14605451
> * Erlang literals are no longer copied when sending messages : This is kinda of a sneaky one. By default (with exception of large binaries) Erlang VM usually copies data when it sends messages. However, in this case module literals (constants, strings, etc) will be another thing that's not copied. There is a hack to dynamically compile configuration values or other tables of constants as a module at runtime. So if you use that hack, you'd get a nice performance boost.

https://www.erlang.org/doc/apps/erts/garbagecollection#literals

https://erlang.org/pipermail/erlang-questions/2017-October/093980.html

