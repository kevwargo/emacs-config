(require 'server)

(setq server-log t)
(unless (server-running-p)
  (server-start))
