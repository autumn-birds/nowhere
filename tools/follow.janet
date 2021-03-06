(def rate-limit 10)                     # in seconds
(var rate-limit-most-recent-call 0)

(def pretty-print pp)

(defn do-rate-limited [funct]
  (let [been-awhile (- (os/time) rate-limit-most-recent-call)]
    (if (< been-awhile rate-limit-most-recent-call)
      (os/sleep (- rate-limit been-awhile))))

  (def result (funct))
  (set rate-limit-most-recent-call (os/time))
  result)

(defn shell-out [args]
  # Return a 3-tuple of returncode, stdout, stderr
  (def proc (os/spawn args :p {:out :pipe :err :pipe}))
  (:wait proc)
  ~[,(proc :return-code)
    ,(:read (proc :out) :all)
    ,(:read (proc :err) :all)])

(defn cmd-output-fail-fast [args]
  # either (yield) a buffer of stdout if the command returned
  # zero, or (error) a buffer of stderr if the command did
  # not.
  # this function should be wrapped in (string (c-o-f-f ...)) if
  # a normal string is required.
  (let [result (shell-out args)]
    (if (= 0 (0 result))
      #(yield (1 result))
      (1 result)
      (error (2 result)))))

(import ./sqlite3 :as sql)

# TODO: Figure out how to store a "what version of the schema is this" in the DB, so if in the future we add something or remove something, the program can run the appropriate addition or removal commands.  (..in that case making a new DB would become the sequence of creating the original schema then amending it n times, but..)
(def db (sql/open (string (cmd-output-fail-fast '("mktemp")))))
(sql/eval db `CREATE TABLE subscriptions(id INTEGER PRIMARY KEY, name TEXT UNIQ, service TEXT, url TEXT, lastChecked INTEGER, checkInterval INTEGER)`)
(sql/eval db `CREATE TABLE posts(id INTEGER PRIMARY KEY, srcId INTEGER, title TEXT, textBody TEXT, date INTEGER, markedUnread INTEGER)`)
(sql/eval db `CREATE TABLE blobs(id INTEGER PRIMARY KEY, srcPostId INTEGER, title TEXT, recipe TEXT, size INTEGER, dateFetched INTEGER, storePath TEXT)`)
# TODO: record hashes?

(defn subscribe-to [name svc url]
  (sql/eval db `INSERT INTO subscriptions VALUES(NULL, :name, :service, :url, :lastChecked, :checkInterval)`
    {:name name
     :service svc
     :url url
     :lastChecked 0
     :checkInterval (* 60 60)}))
# for testing
(subscribe-to "etho" "youtube" "https://youtube.com/c/Ethoslab")
(subscribe-to "grian" "youtube" "https://youtube.com/c/Grian")

(defn add-faux-post [name subscription text]
  (def sub-id-candidates (sql/eval db `SELECT id FROM subscriptions WHERE name IS :name` {:name subscription}))
  (assert (= 1 (length sub-id-candidates)))
  (def [{:id sub-id}] sub-id-candidates)
  (sql/eval db `INSERT INTO posts VALUES(NULL, :srcId, :title, :textBody, 0, 1)` {:srcId sub-id :title name :textBody text}))
# for testing
(add-faux-post "HermitCraft S9E0" "etho" "it's a hermitcraft video, dude, just... chill, okay?")
(add-faux-post "LP episode 999" "etho" "haha, joke's on you")
(add-faux-post "Hermitcraft S9E57" "grian" "Lorem ipsum sit dolor amicus ret, alendil macro nebuli vog no reparilicus; quese, alarminu kishe eharu noburo, et alchismetr do toupu quonu shandor tis. Lorem ipsum sit dolor amicus ret, alendil macro nebuli vog no reparilicus; quese, alarminu kishe eharu noburo, et alchismetr do toupu quonu shandor tis. Lorem ipsum sit dolor amicus ret, alendil macro nebuli vog no reparilicus; quese, alarminu kishe eharu noburo, et alchismetr do toupu quonu shandor tis. Lorem ipsum sit dolor amicus ret, alendil macro nebuli vog no reparilicus; quese, alarminu kishe eharu noburo, et alchismetr do toupu quonu shandor tis. Lorem ipsum sit dolor amicus ret, alendil macro nebuli vog no reparilicus; quese, alarminu kishe eharu noburo, et alchismetr do toupu quonu shandor tis.")

# (pretty-print (sql/eval db `SELECT * FROM posts WHERE srcId = 1;`))

# <GLOBAL TUI STATE>
(var view @[])
(var view-kind :subscriptions)
(var reply-offset-prefix "\t")
(def items-per-page 10)
# </GLOBAL TUI STATE>

(defn get-user-specified-item [number]
  # Will return nil if the item doesn't exist.
  (unless (nil? number) (get view (- number 1))))

(defn show-subscription [subscription index]
  (let [{:name subn :service svc :id subid} subscription
        posts (sql/eval db `SELECT * FROM posts WHERE srcId = :id AND markedUnread = 1` {:id subid})]
    (file/write stdout (string reply-offset-prefix
                               index " "
                               ":" svc ":" subn
                               ": " (length posts) " new posts" "\n"))))

(defn show-post [post index]
  (let [{:title postn :srcId postsid} post
        post-source (sql/eval db `SELECT * FROM subscriptions WHERE id = :id;` {:id postsid})
	{:service p-s-svc :name p-s-name :markedUnread p-s-unread} (first post-source)]
    (assert (= (length post-source) 1))
    (file/write stdout (string reply-offset-prefix
                               index " "
			       (if p-s-unread "[*]")
			       ":" p-s-svc ":" p-s-name ":" postn
			       "\n"))))

(defn do-redisplay []
  # TODO: item count displayed limit; paging logic ...
  (def viewer (match view-kind
    :subscriptions show-subscription
    :posts show-post))
  (var index 0)
  (if (nil? viewer)
    (file/write stdout (string "error: do-redisplay: bad view-kind: " view-kind "\n"))
    (each item view
      (set index (+ index 1))
      (viewer item index))))

(defn setview [kind]
  (match kind
    :subscriptions (do
      (set view (sql/eval db `SELECT * FROM subscriptions;`))
      (set view-kind :subscriptions))
    _ (error (string "setview: bad kind of view " kind))))

(def user-commands @{})
(defmacro defcmd [name help & body]
  ~(set (user-commands ,(keyword name)) {
     :help ,help
     :run (fn ,(symbol (string "cmd-" name)) [args] ,;body)
   }))

(defcmd "!" "cut to Janet repl"
  (repl))

(defcmd "?" "list commands & their help text"
  (def defined-cmds
    (map (fn [k] ~[,(string k) ,((user-commands k) :help)])
         (keys user-commands)))
  (def implicit-cmds '(["q" "leave the program"]))
  (each cmdpair (array ;defined-cmds ;implicit-cmds)
    (file/write stdout (string
      reply-offset-prefix (cmdpair 0) ": " (cmdpair 1) "\n"))))

(defcmd "l"
  "[RECOMPUTE] list current subscriptions [opt arg: search term]"
  (if (> (length args) 0)
    (file/write stdout "searching not implemented.\n")
    (do
      (setview :subscriptions)
      (do-redisplay))))

(defcmd "o"
  "[RECOMPUTE] list posts of a subscription"
  (var err nil)
  (unless (= view-kind :subscriptions)
    (file/write stdout "(relisting all subscriptions first)\n")
    (setview :subscriptions))
  (set err (or err
               (if (zero? (length (string/trim args))) "no arguments")))
  (set err (or err
               (if (nil? (scan-number (string/trim args))) "argument must be a number to reference subscription")))
  (if err
    (file/write stdout (string "sorry, " err "\n"))
    (do
      (def requested-subscription (get-user-specified-item (scan-number (string/trim args))))
      (if (nil? requested-subscription)
        (file/write stdout (string "no such subscription: " (string/trim args) "\n"))
	(do
          (set view (sql/eval db `SELECT * FROM posts WHERE srcId = :id;` {:id (get requested-subscription :id)}))
          (set view-kind :posts)
          (do-redisplay))))))

(defn run-prompt [prompt-fun initial-prompt-string]
  # prompt-fun, called on a string of the user's input, SHOULD return, in a struct:
  #  - either a lambda with which to replace prompt-fun, or nil
  #  - either a prompt-string with which to replace initial-prompt-string, or nil
  #  - either a lambda/closure to call to carry out the command or give any error reports, or nil (indicating no action was forthcoming from interpretation of the input)
  #  - either a truthy value (indicating that the command loop should be broken without taking any action, even if indicated) or nil
  # the struct's keys are [:successor :new-prompt-string :action :stop-now]
  #
  # The string passed to prompt-fun will never be empty.
  # A nil value from prompt-fun will be interpreted equivalent to no string having been entered by the user at all.
  #
  # prompt-fun should be mainly about looking up what command logic to run, if any, and should capture any context required (like the command arguments) inside the :action closure.
  (var continue? true)
  (var prompt initial-prompt-string)
  (var current-prompt-fun prompt-fun)
  (while continue?
    (file/write stdout initial-prompt-string)
    (def input-raw (file/read stdin :line))
    (def input (unless (nil? input-raw) (string/trim input-raw)))
    (def result (cond
      (nil? input) {:stop-now true}
      (= (length input) 0) nil
      (current-prompt-fun input)))
    (unless (nil? result)
      (let [{:successor new-prompt-fun
             :new-prompt-string new-prompt
	     :action fun
	     :stop-now done} result]
        (unless (nil? new-prompt-fun) (set current-prompt-fun new-prompt-fun))
	(unless (nil? new-prompt) (set prompt new-prompt))
	(set continue? (not done))
	(if (and continue?
	         (not (nil? fun)))
          (fun))))))

(run-prompt (fn [promptstring]
  (let [cmd (string/slice promptstring 0 1)
        args (string/slice promptstring 1 (length promptstring))
	cmdstruct (user-commands (keyword cmd))
	cmdfun (unless (nil? cmdstruct) (cmdstruct :run))]
    (cond
      (= "q" cmd) {:stop-now true}
      (nil? cmdfun) {:action (fn [] (file/write stdout "command not found\n"))}
      {:action
        (fn []
	  (cmdfun (string/trim args)))}))) ">")