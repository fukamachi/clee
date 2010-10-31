(in-package :clee-ffi)

(define-foreign-library libevent
  (:darwin "libevent.dylib")
  (:unix "libevent.so")
  (:windows "libevent.dll")
  (t (:default "libevent")))

(load-foreign-library 'libevent)

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
  "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                  for index = 0 then (cl:1+ index)
                  when (cl:listp value) do (cl:setf index (cl:second value)
                                              value (cl:first value))
                    collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here

(cl:defconstant EVLIST_TIMEOUT #x01)

(cl:defconstant EVLIST_INSERTED #x02)

(cl:defconstant EVLIST_SIGNAL #x04)

(cl:defconstant EVLIST_ACTIVE #x08)

(cl:defconstant EVLIST_INTERNAL #x10)

(cl:defconstant EVLIST_INIT #x80)

(cl:defconstant EVLIST_ALL (cl:logior #xf000 #x9f))

(cl:defconstant EV_TIMEOUT #x01)

(cl:defconstant EV_READ #x02)

(cl:defconstant EV_WRITE #x04)

(cl:defconstant EV_SIGNAL #x08)

(cl:defconstant EV_PERSIST #x10)

(cffi:defcstruct event
  (min-heap-idx :unsigned-int)
  (ev-base :pointer)
  (ev-fd :int)
  (ev-events :short)
  (ev-ncalls :short)
  (ev-pncalls :pointer)
  (ev-timeout :pointer)
  (ev-pri :int)
  (ev-callback :pointer)
  (ev-arg :pointer)
  (ev-res :int)
  (ev-flags :int)
  (ev-signal-next :pointer)
  (ev-active-next :pointer)
  (ev-next :pointer))

(cffi:defcstruct event-ev-signal-next
  (tqe-next :pointer)
  (tqe-prev :pointer))

(cffi:defcstruct event-ev-active-next
  (tqe-next :pointer)
  (tqe-prev :pointer))

(cffi:defcstruct event-ev-next
  (tqe-next :pointer)
  (tqe-prev :pointer))

(cffi:defcstruct evkeyval
  (key :string)
  (value :string)
  (next :pointer))

(cffi:defcstruct evkeyval-next
  (tqe-next :pointer)
  (tqe-prev :pointer))

(cffi:defcfun ("event_base_new" event-base-new) :pointer)

(cffi:defcfun ("event_init" event-init) :pointer)

(cffi:defcfun ("event_reinit" event-reinit) :int
  (base :pointer))

(cffi:defcfun ("event_dispatch" event-dispatch) :int)

(cffi:defcfun ("event_base_dispatch" event-base-dispatch) :int
  (arg0 :pointer))

(cffi:defcfun ("event_base_get_method" event-base-get-method) :string
  (arg0 :pointer))

(cffi:defcfun ("event_base_free" event-base-free) :void
  (arg0 :pointer))

(cl:defconstant _EVENT_LOG_DEBUG 0)

(cl:defconstant _EVENT_LOG_MSG 1)

(cl:defconstant _EVENT_LOG_WARN 2)

(cl:defconstant _EVENT_LOG_ERR 3)

(cffi:defcfun ("event_set_log_callback" event-set-log-callback) :void
  (cb :pointer))

(cffi:defcfun ("event_base_set" event-base-set) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cl:defconstant EVLOOP_ONCE #x01)

(cl:defconstant EVLOOP_NONBLOCK #x02)

(cffi:defcfun ("event_loop" event-loop) :int
  (arg0 :int))

(cffi:defcfun ("event_base_loop" event-base-loop) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("event_loopexit" event-loopexit) :int
  (arg0 :pointer))

(cffi:defcfun ("event_base_loopexit" event-base-loopexit) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("event_loopbreak" event-loopbreak) :int)

(cffi:defcfun ("event_base_loopbreak" event-base-loopbreak) :int
  (arg0 :pointer))

(cffi:defcfun ("event_set" event-set) :void
  (ev :pointer)
  (fd :int)
  (flags :short)
  (callback :pointer)
  (arg :pointer))

(cffi:defcfun ("event_once" event-once) :int
  (arg0 :int)
  (arg1 :short)
  (arg2 :pointer)
  (arg3 :pointer)
  (arg4 :pointer))

(cffi:defcfun ("event_base_once" event-base-once) :int
  (base :pointer)
  (fd :int)
  (events :short)
  (callback :pointer)
  (arg :pointer)
  (timeout :pointer))

(cffi:defcfun ("event_add" event-add) :int
  (ev :pointer)
  (timeout :pointer))

(cffi:defcfun ("event_del" event-del) :int
  (arg0 :pointer))

(cffi:defcfun ("event_active" event-active) :void
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :short))

(cffi:defcfun ("event_pending" event-pending) :int
  (ev :pointer)
  (event :short)
  (tv :pointer))

(cffi:defcfun ("event_get_version" event-get-version) :string)

(cffi:defcfun ("event_get_method" event-get-method) :string)

(cffi:defcfun ("event_priority_init" event-priority-init) :int
  (arg0 :int))

(cffi:defcfun ("event_base_priority_init" event-base-priority-init) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("event_priority_set" event-priority-set) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcstruct evbuffer
  (buffer :pointer)
  (orig-buffer :pointer)
  (misalign :pointer)
  (totallen :pointer)
  (off :pointer)
  (cb :pointer)
  (cbarg :pointer))

(cl:defconstant EVBUFFER_READ #x01)

(cl:defconstant EVBUFFER_WRITE #x02)

(cl:defconstant EVBUFFER_EOF #x10)

(cl:defconstant EVBUFFER_ERROR #x20)

(cl:defconstant EVBUFFER_TIMEOUT #x40)

(cffi:defcstruct event-watermark
  (low :pointer)
  (high :pointer))

(cffi:defcstruct bufferevent
  (ev-base :pointer)
  (ev-read event)
  (ev-write event)
  (input :pointer)
  (output :pointer)
  (wm-read event-watermark)
  (wm-write event-watermark)
  (readcb :pointer)
  (writecb :pointer)
  (errorcb :pointer)
  (cbarg :pointer)
  (timeout-read :int)
  (timeout-write :int)
  (enabled :short))

(cffi:defcfun ("bufferevent_new" bufferevent-new) :pointer
  (fd :int)
  (readcb :pointer)
  (writecb :pointer)
  (errorcb :pointer)
  (cbarg :pointer))

(cffi:defcfun ("bufferevent_base_set" bufferevent-base-set) :int
  (base :pointer)
  (bufev :pointer))

(cffi:defcfun ("bufferevent_priority_set" bufferevent-priority-set) :int
  (bufev :pointer)
  (pri :int))

(cffi:defcfun ("bufferevent_free" bufferevent-free) :void
  (bufev :pointer))

(cffi:defcfun ("bufferevent_setcb" bufferevent-setcb) :void
  (bufev :pointer)
  (readcb :pointer)
  (writecb :pointer)
  (errorcb :pointer)
  (cbarg :pointer))

(cffi:defcfun ("bufferevent_setfd" bufferevent-setfd) :void
  (bufev :pointer)
  (fd :int))

(cffi:defcfun ("bufferevent_write" bufferevent-write) :int
  (bufev :pointer)
  (data :pointer)
  (size :pointer))

(cffi:defcfun ("bufferevent_write_buffer" bufferevent-write-buffer) :int
  (bufev :pointer)
  (buf :pointer))

(cffi:defcfun ("bufferevent_read" bufferevent-read) :pointer
  (bufev :pointer)
  (data :pointer)
  (size :pointer))

(cffi:defcfun ("bufferevent_enable" bufferevent-enable) :int
  (bufev :pointer)
  (event :short))

(cffi:defcfun ("bufferevent_disable" bufferevent-disable) :int
  (bufev :pointer)
  (event :short))

(cffi:defcfun ("bufferevent_settimeout" bufferevent-settimeout) :void
  (bufev :pointer)
  (timeout-read :int)
  (timeout-write :int))

(cffi:defcfun ("bufferevent_setwatermark" bufferevent-setwatermark) :void
  (bufev :pointer)
  (events :short)
  (lowmark :pointer)
  (highmark :pointer))

(cffi:defcfun ("evbuffer_new" evbuffer-new) :pointer)

(cffi:defcfun ("evbuffer_free" evbuffer-free) :void
  (arg0 :pointer))

(cffi:defcfun ("evbuffer_expand" evbuffer-expand) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("evbuffer_add" evbuffer-add) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("evbuffer_remove" evbuffer-remove) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("evbuffer_readline" evbuffer-readline) :string
  (arg0 :pointer))

(cffi:defcfun ("evbuffer_add_buffer" evbuffer-add-buffer) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("evbuffer_add_printf" evbuffer-add-printf) :int
  (arg0 :pointer)
  (fmt :string)
  &rest)

(cffi:defcfun ("evbuffer_add_vprintf" evbuffer-add-vprintf) :int
  (arg0 :pointer)
  (fmt :string)
  (ap :pointer))

(cffi:defcfun ("evbuffer_drain" evbuffer-drain) :void
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("evbuffer_write" evbuffer-write) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("evbuffer_read" evbuffer-read) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("evbuffer_find" evbuffer-find) :pointer
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("evbuffer_setcb" evbuffer-setcb) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("evtag_init" evtag-init) :void)

(cffi:defcfun ("evtag_marshal" evtag-marshal) :void
  (evbuf :pointer)
  (tag :pointer)
  (data :pointer)
  (len :pointer))

(cffi:defcfun ("encode_int" encode-int) :void
  (evbuf :pointer)
  (number :pointer))

(cffi:defcfun ("evtag_marshal_int" evtag-marshal-int) :void
  (evbuf :pointer)
  (tag :pointer)
  (integer :pointer))

(cffi:defcfun ("evtag_marshal_string" evtag-marshal-string) :void
  (buf :pointer)
  (tag :pointer)
  (string :string))

(cffi:defcfun ("evtag_marshal_timeval" evtag-marshal-timeval) :void
  (evbuf :pointer)
  (tag :pointer)
  (tv :pointer))

(cffi:defcfun ("evtag_unmarshal" evtag-unmarshal) :int
  (src :pointer)
  (ptag :pointer)
  (dst :pointer))

(cffi:defcfun ("evtag_peek" evtag-peek) :int
  (evbuf :pointer)
  (ptag :pointer))

(cffi:defcfun ("evtag_peek_length" evtag-peek-length) :int
  (evbuf :pointer)
  (plength :pointer))

(cffi:defcfun ("evtag_payload_length" evtag-payload-length) :int
  (evbuf :pointer)
  (plength :pointer))

(cffi:defcfun ("evtag_consume" evtag-consume) :int
  (evbuf :pointer))

(cffi:defcfun ("evtag_unmarshal_int" evtag-unmarshal-int) :int
  (evbuf :pointer)
  (need-tag :pointer)
  (pinteger :pointer))

(cffi:defcfun ("evtag_unmarshal_fixed" evtag-unmarshal-fixed) :int
  (src :pointer)
  (need-tag :pointer)
  (data :pointer)
  (len :pointer))

(cffi:defcfun ("evtag_unmarshal_string" evtag-unmarshal-string) :int
  (evbuf :pointer)
  (need-tag :pointer)
  (pstring :pointer))

(cffi:defcfun ("evtag_unmarshal_timeval" evtag-unmarshal-timeval) :int
  (evbuf :pointer)
  (need-tag :pointer)
  (ptv :pointer))
