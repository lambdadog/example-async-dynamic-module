#include <emacs-module.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>

#define UNUSED(x) (void)(x)

/* An asynchronous "submission", which is submitted to a new worker
   thread. */
struct submission {
  unsigned int seconds;
  ptrdiff_t string_len;
  char *string;
  uint32_t callback_num;
};

/* An asynchronous "completion", which is pushed to the C static
   global `completions' variable to be drained by
   `example-async-dynamic-module-dyn--drain-completions' */
struct completion {
  struct completion *next;
  ptrdiff_t string_len;
  char *string;
  uint32_t callback_num;
};

int plugin_is_GPL_compatible;

/* The file descriptor of our pipe process, gotten by calling
   `open_channel' */
static int channel_fd = -1;

/* Used to assign new callback ids */
static uint32_t callback_idx = 0;

/* An internally linked list of completions, to be drained by
   `example-async-dynamic-module-dyn--drain-completions' */
static struct completion *completions = NULL;
static pthread_mutex_t completions_lock = PTHREAD_MUTEX_INITIALIZER;

/* Handles signals and errors by rethrowing/signaling them. If this
   function returns false, cleanup and exit immediately. */
static bool
handle_non_local_exit (emacs_env *env)
{
  enum emacs_funcall_exit result;
  emacs_value symbol;
  emacs_value data;

  result = env->non_local_exit_get (env, &symbol, &data);

  switch (result)
    {
    case emacs_funcall_exit_return:
      return true;

    case emacs_funcall_exit_signal:
      env->non_local_exit_signal (env, symbol, data);
      return false;

    case emacs_funcall_exit_throw:
      env->non_local_exit_throw (env, symbol, data);
      return false;
    }
}

/* Defines a function. */
static void
defun (emacs_env *env, const char *name, emacs_function function,
       ptrdiff_t arity)
{
  emacs_value func;
  emacs_value args[2];

  func = env->make_function (env, arity, arity, function, NULL, NULL);

  args[0] = env->intern (env, name);
  args[1] = func;

  env->funcall (env, env->intern (env, "defalias"), 2, args);
}

/* Provides a feature. */
static void
provide (emacs_env *env, const char *feature_name)
{
  emacs_value args[1];

  args[0] = env->intern (env, feature_name);

  env->funcall (env, env->intern (env, "provide"), 1, args);
}

/* Runs in a new thread as an example of long, blocking work being
   wrapped by an async function. */
static void
*run_sleep_ret (void *ptr)
{
  struct submission *thread_data;

  unsigned int seconds;
  ptrdiff_t string_len;
  char *string;
  uint32_t callback_num;

  struct completion *next_completion;

  thread_data = (struct submission *) ptr;

  seconds = thread_data->seconds;
  string_len = thread_data->string_len;
  string = thread_data->string;
  callback_num = thread_data->callback_num;

  free (thread_data);

  while (seconds > 0)
    seconds = sleep (seconds);

  // Write completion
  next_completion = malloc (sizeof (*next_completion));
  next_completion->string_len = string_len;
  next_completion->string = string;
  next_completion->callback_num = callback_num;

  pthread_mutex_lock (&completions_lock);
  next_completion->next = completions;
  completions = next_completion;
  pthread_mutex_unlock (&completions_lock);

  char zero = 0;
  if (write (channel_fd, (void *) &zero, 1) != -1)
    // If the channel is closed it's essentially impossible to
    // error-handle meaningfully as we have no way to talk to the main
    // process at all, but we can at least skip trying to fdatasync.
    goto fin;
  fdatasync (channel_fd);

 fin:
  return NULL;
}

/* Drains all completions. Is called by the process filter upon being
   notified by a worker thread that completions are available. */
static emacs_value
Fexample_async_dynamic_module_dyn__drain_completions (emacs_env *env,
						      ptrdiff_t nargs,
						      emacs_value *args,
						      void *data)
{
  UNUSED (nargs);
  UNUSED (args);
  UNUSED (data);

  emacs_value Qcons;
  emacs_value result;

  struct completion *cmplt;
  emacs_value cons_args[2];

  if (channel_fd == -1)
    {
      env->non_local_exit_throw (env, env->intern (env, "not-initialized"),
				 env->intern (env, "nil"));
      goto err;
    }

  Qcons = env->intern (env, "cons");
  result = env->intern (env, "nil");

  pthread_mutex_lock (&completions_lock);
  while (completions != NULL)
    {
      cmplt = completions;
      completions = cmplt->next;

      cons_args[0] = env->make_integer (env, (intmax_t) cmplt->callback_num);
      cons_args[1] = env->make_string (env, cmplt->string, cmplt->string_len);

      cons_args[0] = env->funcall (env, Qcons, 2, cons_args);
      cons_args[1] = result;

      result = env->funcall (env, Qcons, 2, cons_args);

      free (cmplt->string);
      free (cmplt);
    }
  pthread_mutex_unlock (&completions_lock);

  return result;
 err:
  return NULL;
}

/* Spawns a thread which sleeps, submits a completion, then notifies
   Emacs. Takes SECONDS and STRING as its arguments, returns a
   "callback id" used to register a callback on the elisp end. */
static emacs_value
Fexample_async_dynamic_module_dyn__sleep_ret (emacs_env *env, ptrdiff_t nargs,
					      emacs_value *args, void *data)
{
  UNUSED (nargs);
  UNUSED (data);

  unsigned int seconds;
  ptrdiff_t string_len;
  char *string = NULL;

  int64_t callback_num = -1;

  struct submission *thread_data = NULL;

  pthread_t thread;

  if (channel_fd == -1)
    {
      env->non_local_exit_throw (env, env->intern (env, "not-initialized"),
				 env->intern (env, "nil"));
      goto err;
    }

  seconds = (unsigned int) env->extract_integer (env, args[0]);
  if (!handle_non_local_exit (env))
    goto err;

  // Get string length
  env->copy_string_contents (env, args[1], NULL, &string_len);

  // Get string
  string = malloc ((size_t) string_len);
  env->copy_string_contents (env, args[1], string, &string_len);

  callback_num = (int64_t) callback_idx++;

  thread_data = malloc (sizeof (*thread_data));
  thread_data->seconds = seconds;
  // string_len includes the null byte
  thread_data->string_len = string_len - 1;
  thread_data->string = string;
  thread_data->callback_num = (uint32_t) callback_num;

  pthread_create (&thread, NULL, run_sleep_ret, (void*) thread_data);

  return env->make_integer (env, (intmax_t) thread_data->callback_num);
 err:
  return NULL;
}

/* Initializes module. Takes a pipe process which will be used to
   notify that there are available completions as its argument. */
static emacs_value
Fexample_async_dynamic_module_dyn__init (emacs_env *env, ptrdiff_t nargs,
					 emacs_value *args, void *data)
{
  UNUSED (nargs);
  UNUSED (data);

  emacs_value channel;

  if (channel_fd != -1)
    {
      env->non_local_exit_throw (env, env->intern (env, "already-initialized"),
				 env->intern (env, "nil"));
      goto err;
    }

  channel = args[0];

  channel_fd = env->open_channel (env, channel);

  return env->intern (env, "nil");
 err:
  return NULL;
}

/* Initializes the module. */
int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env;

  env = runtime->get_environment (runtime);

  if ((unsigned long) env->size < sizeof (struct emacs_env_28)) {
    env->non_local_exit_throw (env, env->intern (env, "module-requires-emacs-28.1+"),
			       env->intern (env, "nil"));
    goto err;
  }

  // Register our functions
  defun (env, "example-async-dynamic-module-dyn--init",
	 Fexample_async_dynamic_module_dyn__init, 1);

  defun (env, "example-async-dynamic-module-dyn--sleep-ret",
	 Fexample_async_dynamic_module_dyn__sleep_ret, 2);

  defun (env, "example-async-dynamic-module-dyn--drain-completions",
	 Fexample_async_dynamic_module_dyn__drain_completions, 0);

  // Provide the `example-async-dynamic-module-dyn' feature.
  provide (env, "example-async-dynamic-module-dyn");

  return 0;
 err:
  return 1;
}
