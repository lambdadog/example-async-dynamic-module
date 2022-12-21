# example-async-dynamic-module
An example of using
[`open_channel`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Misc.html#index-open_005fchannel)
to implement asynchronous functions in emacs dynamic modules.

Obviously a particularly contrived example and in the real world you
would probably prefer to use a thread pool of some sort, but works.

## Currently unimplemented
 - Handling `kill-process` on our pipe process (or the pipe just
   breaking, for whatever reason)
   + This could be done by adding a deinit function in our dynamic
	 module and installing a process sentinel on the pipe process that
	 detects being killed and calls the dynamic module's deinit
	 function.
