;nyquist plug-in
;version 3
;type process
;categories "http://lv2plug.in/ns/lv2core#FunctionPlugin"
;name "De-Clicker"
;action "Calculating..."
;info "Detects brief spikes in amplitudes of frequency components,\nand produces labels, or applies filters to repair the sound.\n\nDefault settings make twelve frequency bands of one-half octave each."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declicker settings

;control control-action-choice "Action" choice "Isolate changes,Apply changes,Debug Labels (short),Debug Labels (long)" 1
;control control-passes "Number of passes" int "" 2 1 4
;control control-relative-threshold "Sensitivity threshold (dB)\n(lower detects more clicks)" real "" 6 0.5 42
;control control-step-dur-ms "Step size (ms)\n(precision of repair intervals)" real "" 5 1 1000
;control control-max-steps "Maximum click length (steps)" int "" 2 1 250
;control control-separation "Minimum time between clicks (steps)" int "" 3 1 100
;control control-crackle-threshold "Dense click threshold (dB)\n(allow detection of narrowly\nseparated clicks below this)" real "" -45 -200 0

;control control-frequency-bound1 "Test frequencies between" real "Hz" 150 20 22050

;control control-frequency-bound2 "... and" real "Hz" 9600 20 22050

;control control-n-bands "Number of frequency bands" int "" 12 1 50

;control control-crossfade-interval "Widen repair intervals\nat each end by (ms)" real "" 5 0 1000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; obsolete settings
(setf control-band-type 0)
(setq control-absolute-threshold -200.0)
(setq control-fix-type 0)
;; control control-band-type "Bands are" choice "logarithmic,linear" 0
;; control control-absolute-threshold "Minimum peak in frequency band (dB)\n(lower increases sensitivity)" real "" -200 -200 0
;;control control-fix-type "Soften to" choice "Background,Relative Threshold" 0

;; Thought I should hide one of the less often used controls, it's already too confusing
;;control control-rise-time "Max rise/fall time (steps)" int "" 1 1 100
(setq control-rise-time 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation switches not in the UI

;; Try to improve frequency band resolution, at the expense of more compute
;; time
(setq use-large-windows t)
;; Different method for "large" windows that are less large
(setq use-double-convolution nil)
;; When using large windows, compensate for the blurring of clicks in time
(setq use-threshold-corrections nil)
;; Which convolution window to use?  See association list below
(setq use-window-choice
;;      'rectangle
;;      'FTSRS
      'SFT3F
)

;; Make detection bands twice as wide (in log frequency) as the repair
;; bands.  Thus a click can influence two sliders, above and below it
;; in frequency.  This also makes convolution windows shorter for less
;; compute time.
(setq use-wide-detection-bands t)

;; If true, this may slightly improve the equalization "curves" used in repairs
(setq use-late-steepness-test t)

;; As of writing, Nyquist has a bug that crashes certain compositions of
;; convolve and snd-avg!  See discussion here:
;; http://forum.audacityteam.org/viewtopic.php?f=39&t=75738
(setq use-crash-workaround t)

;; Debug switches for displaying certain intermediate sound results, but only
;; the first non-nil one applies, and only to one test frequency
(setq debug-convolution-window nil)
(setq debug-convolution nil)
(setq debug-peaks nil)

;; control certain diagnostics
(setq debug-messages t)

;; Choose the band for debugging; skip so many lower bands, then draw the
;; curve for that band.
(setq debug-band 0)

;; various other experiments...
(setq use-stagger t)
(setq use-skewed-separation t)
(setq debug-overshoot nil)

(setq stricter-test nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities

(setq LOG2 (log 2.0))

(defmacro add-to (x val) `(setf ,x (+ ,val ,x)))

(defmacro update-op (op x val)
  (let ((temp1 (gensym))
        (temp2 (gensym)))
    `(let ((,temp1 ,val)
           (,temp2 ,x))
       (setf ,x (if ,temp2 (,op ,temp2 ,temp1) ,temp1)))))
(defmacro update-max (x val) `(update-op max ,x ,val))
(defmacro update-min (x val) `(update-op min ,x ,val))

(defmacro push-back-cons (cell tail)
  `(setf ,tail (last (rplacd ,tail ,cell))))
(defmacro push-back (val tail)
  `(push-back-cons (cons ,val nil) ,tail))

 ;; Avoid lots of gc for frequently recycled data, which have the form of
;; dotted pairs, the cdr reserved for linking.
(defun init-free-list (cell) (cons cell cell))
(defun push-free-list (cell queue) (push-back-cons cell (cdr queue)))
(defun pop-free-list (queue)
  (if (null (cdar queue))
      nil
      (rplacd (prog1 (car queue)
                (pop (car queue)))
              nil)))

;;; I was so annoyed at the lack of a loop that does not test until after the
;;; first iteration...
(defun loop1
  (head bindings termination body-forms)
  (let ((initialized (gensym)))
    (flet
        ((expand-binding
          (binding)
          (if (atom binding)
              binding
              `(,(car binding) nil ,@(cdr binding)))))
      (let ((expanded-bindings (mapcar #'expand-binding bindings)))
        `(,head
          (,initialized ,@expanded-bindings)
          ((and ,initialized ,(car termination)) ,@(cdr termination))
          (cond (,initialized ,@body-forms)
                (t (setf ,initialized t))))))))
(defmacro do1 (bindings termination &rest body-forms)
  (loop1 'do bindings termination body-forms))
(defmacro do1* (bindings termination &rest body-forms)
  (loop1 'do* bindings termination body-forms))

(defmacro loop-build-list
  (word bindings test init builder &rest body-forms)
  (let ((head (gensym))
        (lis (gensym))
        (tail (gensym)))
    ;; put the loop in a block so that body-forms
    ;; can use return to stop accumulating
    `(let* ((,head ,init)
            (,lis (or ,head (list nil)))
            (,tail ,lis))
       (block nil (,word ,bindings
                         (,test)
                         (,builder (progn ,@body-forms) ,tail)))
       (or ,head (cdr ,lis)))))
(defmacro do-build-list (bindings test &rest body-forms)
  `(loop-build-list do ,bindings ,test nil push-back ,@body-forms))
(defmacro do*-build-list (bindings test &rest body-forms)
  `(loop-build-list do* ,bindings ,test nil push-back ,@body-forms))
(defmacro do1-build-list (bindings test &rest body-forms)
  `(loop-build-list do1 ,bindings ,test nil push-back ,@body-forms))
(defmacro do1*-build-list (bindings test &rest body-forms)
  `(loop-build-list do1* ,bindings ,test nil push-back ,@body-forms))

(defmacro do-link-list (head bindings test &rest body-forms)
  `(loop-build-list do ,bindings ,test ,head push-back-cons ,@body-forms))
(defmacro do*-link-list (head bindings test &rest body-forms)
  `(loop-build-list do* ,bindings ,test ,head push-back-cons ,@body-forms))
(defmacro do1-link-list (head bindings test &rest body-forms)
  `(loop-build-list do1 ,bindings ,test ,head push-back-cons ,@body-forms))
(defmacro do1*-link-list (head  bindings test &rest body-forms)
  `(loop-build-list do1* ,bindings ,test ,head push-back-cons ,@body-forms))

;; add to .emacs
;; (put 'do1 'lisp-indent-function 2)
;; (put 'do1* 'lisp-indent-function 2)
;; (put 'do-build-list 'lisp-indent-function 2)
;; (put 'do*-build-list 'lisp-indent-function 2)
;; (put 'do1-build-list 'lisp-indent-function 2)
;; (put 'do1*-build-list 'lisp-indent-function 2)
;; (put 'do-link-list 'lisp-indent-function 3)
;; (put 'do*-link-list 'lisp-indent-function 3)
;; (put 'do1-link-list 'lisp-indent-function 3)
;; (put 'do1*-link-list 'lisp-indent-function 3)
;; (defun my-elisp-mode-keywords()
;;   (font-lock-add-keywords 'lisp-mode
;;     '(("(\\(\\<do1\\>\\)" . 1)
;;       ("(\\(\\<do1\\*\\>\\)" . 1)

;;       ("(\\(\\<do-build-list\\>\\)" . 1)
;;       ("(\\(\\<do1-build-list\\>\\)" . 1)
;;       ("(\\(\\<do\\*-build-list\\>\\)" . 1)
;;       ("(\\(\\<do1\\*-build-list\\>\\)" . 1))))

;;       ("(\\(\\<do-link-list\\>\\)" . 1)
;;       ("(\\(\\<do1-link-list\\>\\)" . 1)
;;       ("(\\(\\<do\\*-link-list\\>\\)" . 1)
;;       ("(\\(\\<do1\\*-link-list\\>\\)" . 1))))
;; (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-keywords)

(defmacro initially (flag &rest forms)
  `(when (not ,flag) (setf ,flag t) ,@forms))

(defun geometric-mean (x y) (sqrt (* x y)))

;; Which of two implementations works faster?  I am not sure.
(defun trim-snd (snd start dur)
  (prod snd (snd-const 1.0 start (snd-srate snd) dur)))
;; (defun trim-snd (snd start dur)
;;   (let* ((t0 (snd-t0 snd))
;;          (st (+ start (- t0) start))
;;          (en (+ st dur)))
;;     (snd-xform snd (snd-srate snd) start st en 1.0)))

(defun cosine-periods (srate freq n-periods)
  (snd-osc (first *sine-table*) (second *sine-table*)
           srate freq 0.0 (/ n-periods freq) 90.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compute a convolution of sounds by dividing the response into a sum
;;; of functions over disjoint intervals, convolving with each, and adding.
;;; Why?  Because I must work around a bug in Nyquist at the time of this
;;; writing, which happens when you convolve with a response that is "too wide"
;;; (I don't know how exactly to define that but I have a guess), and then
;;; pass that to snd-avg, and then evaluate the result.  So, avoid
;;; too-large windows in convolve.  Discussion of the crash bug here:
;;; http://forum.audacityteam.org/viewtopic.php?f=39&t=75738
(defun convolve-by-pieces (snd response n-pieces)
  (if (= n-pieces 1)
      (convolve snd response)
      (let* ((t0 (snd-t0 response))
             (srate (snd-srate response))
             (response-length (snd-length response ny:all))
             (response-dur (/ response-length srate))
             (step-length (/ response-length n-pieces))
             ;; Make this a multiple of sample time, to be safe.
             (step-dur (/ step-length srate)))
        (do ((time t0 (+ time step-dur))
             (ii 0 (1+ ii))
             temp)
            ((= ii n-pieces) (apply #'sum temp))
          (let* ((response-piece
                  (snd-xform response srate t0 time
                             (if (= ii (1- n-pieces))
                                 ;; The last piece may be longer, it gets
                                 ;; all of the remainder of the division that
                                 ;; defined step-length
                                 (+ t0 response-dur)
                                 (+ time step-dur)) 1.0))
                 (unshifted-piece (convolve snd response-piece))
                 (piece (shift-time unshifted-piece (- time t0))))
            (push piece temp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define functions for convolution to isolate frequency bands

;; I take data and abbreviated names for several of the windows defined in
;; Appendix D of: http://edoc.mpg.de/395068
;;
;; I have not experimented with all in this list or included all from the
;; paper.  Feel free to modify.
;;
;; Following list contains coefficients of constant and cosines, and
;; the first zero in bins, and the 3dB width in bins.  The last two define
;; two different criteria for "width" of a lobe.
;;
;; There are two classes of flat top windows, some optimized for fast
;; decay of side lobes, others for minimum height of side lobes.
;;
;; Of the ones below SFT3F has the
;; highest ratio of 3dB width to first zero, which in some sense makes its
;; central lobe "steepest" for a given width, which is why I suspect it may
;; be the best choice for isolating a frequency band
;; (even though its highest side lobe is "only" as good as Hann).
;; By this criterion it is better than the fast decaying flat tops with greater
;; numbers of terms.

(setq
 window-function-list
 '((rectangle
    . ((1.0) 1.0 0.8445))      ; highest sidelobe -13.3dB at 1.43 bins
   (Hann
    . ((0.5 -0.5) 2.0 1.4382)) ; highest sidelobe -31.5dB at 2.362 bins
   (Hamming
    . ((0.54 -0.46) 2.0 1.3008)) ; higest sidelobe -42.7dB at 4.50 bins
   (FTSRS                         ; Stanford Research
    . ((1.0 -1.93 1.29 -0.388 0.028) 4.719 3.7274))
   ;; see also http://en.wikipedia.org/wiki/Window_function#Flat_top_window
                                        ; highest sidelobe -76.6dB at 5.37 bins
   (SFT3F
    . ((0.26526 -0.5 0.23474) 3.0 3.1502))
                                        ; highest sidelobe -31.7dB at 3.369 bins
   (SFT3M
    . ((0.28235 -0.52105 0.19659) 3.0 2.9183)) ; SFT3M
                                        ; highest sidelobe -44.2dB at 5.5 bins
   (FTNI
    . ((0.2810639 -0.5208972 0.1980399) 3.0 2.9355))))
                                        ; highest sidelobe -44.4 dB at 3.23 bins

(defmacro window-function-coefficients (data) `(first ,data))
(defmacro window-function-first-zero (data) `(second ,data))
(defmacro window-function-3dB-width (data) `(third ,data))

(defun sum-of-cosines-window (window-choice srate dur)
  (let* ((data (cdr (assoc window-choice window-function-list)))
         (coefficients (window-function-coefficients data))
         (freq (/ dur)))
    (do ((ii 1 (1+ ii))
         (remaining (cdr coefficients) (cdr remaining))
         (result (snd-const (car coefficients) 0.0 srate dur)
                 (sum result
                      (prod (car remaining)
                            (cosine-periods srate (* ii freq) ii)))))
        ((not remaining) result))))

(defun normalize-convolution-window (window frequency)
  (let* ((srate (snd-srate window))
         (window-size (snd-length window ny:all))
         (window-dur (/ window-size srate))
         (period (/ frequency))
         (n-periods (round (/ window-dur period)))
         (new-frequency (/ n-periods window-dur))
         (cosines (if (plusp n-periods)
                      (cosine-periods srate new-frequency n-periods)
                      1.0))
         ;; the coefficient is such that convolving the window with
         ;; a sinusoid of the same frequency would leave amplitude unchanged.
         ;; That is important to make the absolute threshold test work
         ;; correctly.
         ;; The sign is simply to avoid a 180 degree phase change, unimportant
         ;; for the effects but nicer when debugging the curve.
         (scalar
          (/ (if (oddp n-periods) -1.0 1.0)
             (* window-size (snd-fetch
                             (snd-avg (prod cosines window)
                                      window-size window-size OP-AVERAGE))))))
    (prod scalar window)))

(defun compute-convolution-window (srate bin-frequency bin-number)
  (let* ((bin-period (/ bin-frequency))
         (frequency (* bin-frequency bin-number))
         (window-size (truncate (* bin-period srate)))
         (_ (when debug-messages
              (terpri)
              (print
               (format nil "~A window: ~A samples ~A periods ~A Hz"
                       use-window-choice window-size bin-number frequency))))
         (cosines (if (plusp bin-number)
                      (cosine-periods srate frequency bin-number)
                      1.0))
         (window-function
          (sum-of-cosines-window use-window-choice srate bin-period)))
    (prod cosines window-function)))

;; Alternative definition that composes two responses that act as "high pass"
;; and "low pass" near the desired band.
(defun compute-double-convolution-window
  (srate band-bottom band-top)
  (let* ((band-ratio (/ band-top band-bottom))
         ;(sqrt-band-ratio (sqrt band-ratio))
         (max-zero-above (* band-top band-ratio))
         (min-zero-below (/ band-bottom band-ratio))

         (data (cdr (assoc use-window-choice window-function-list)))
         (first-zero (window-function-first-zero data))
         (3dB-half-width (/ (window-function-3dB-width data) 2))
         (slope-width (- first-zero 3db-half-width))
         (n-cosines (1- (length (window-function-coefficients data))))

         (max-lowpass-bin (/ (- max-zero-above band-top) slope-width))
         (lowpass-bin-number
          (1+ (truncate (/ (- band-top (* 3db-half-width max-lowpass-bin))
                           max-lowpass-bin))))
         (lowpass-bin (/ band-top (+ lowpass-bin-number 3dB-half-width)))
         (window1
          (compute-convolution-window srate lowpass-bin lowpass-bin-number))

         (nyquist (/ srate 2))
         ;; Don't go so high that the components of the window get aliased
         (max-highpass-bin (min (/ nyquist (max 1 n-cosines))
                                (/ (- band-bottom min-zero-below) slope-width)))
         (highpass-bin-number
          (1+ (truncate (/ (+ band-bottom (* 3db-half-width max-highpass-bin))
                           max-highpass-bin))))
         (highpass-bin (/ band-bottom (- highpass-bin-number 3dB-half-width)))

         (_ (when debug-messages
              (print (list nyquist max-highpass-bin highpass-bin
                           highpass-bin-number))))
         (window2
          (compute-convolution-window srate highpass-bin highpass-bin-number))
         (window3 (convolve window1 window2)))
    ;; convolve makes the last sample always zero.  Snip it off.
    (snd-xform
     window3 srate 0.0 0.0 (/ (1- (snd-length window3 ny:all)) srate) 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find clicks at one frequency

;;; constructor and accessors
(defmacro click-detail-time (detail) ; Hz
  `(aref (car ,detail) 0))
(defmacro click-detail-frequency (detail) ; Hz
  `(aref (car ,detail) 1))
(defmacro click-detail-steps (detail) ; integer to mulitply by step duration
  `(aref (car ,detail) 2))
(defmacro click-detail-ratio (detail) ; float (dimensionless)
  `(aref (car ,detail) 3))
(defmacro click-detail-difference (detail) ; float (dB)
  `(linear-to-db (click-detail-ratio detail)))

(setf click-detail-free-list (init-free-list (cons (make-array 4) nil)))
(defun click-detail (time frequency steps ratio1 ratio2)
  (let ((ratio
         ;; at first I used geometric mean, but decided minimum was much
         ;; better to avoid cutting stop consonants too much.  A stop
         ;; consonant has a very asymmetrical step up in many frequencies,
         ;; and often only some of them are harsh from overshooting the
         ;; plateau that follows.  Correct only that overshoot.
         ;;(geometric-mean ,ratio1 ,ratio2)
         (min ratio1 ratio2))
        (cell (pop-free-list click-detail-free-list)))
    (if cell
        (let ((v (car cell)))
          (setf (aref v 0) time
                (aref v 1) frequency
                (aref v 2) steps
                (aref v 3) ratio)
          cell)
        (cons (vector time frequency steps ratio) nil))))

(defun free-click-detail (detail)
  (push-free-list detail click-detail-free-list))

(defun compute-crackle-thresholds
  (crackle-threshold-db separation rise-time)
  ;; in future, may try something more complicated,
  ;; raising threshold with greater indices
  (let ((array
         (make-array (1- (max rise-time (+ separation separation)))))
        (crackle-threshold (db-to-linear crackle-threshold-db)))
    (dotimes (ii (length array))
      (setf (aref array ii) crackle-threshold))
    array))

(labels
    ((one-side-separation
      (separation)
      (if use-skewed-separation
          (+ separation (/ separation 3))
          ;;(+ separation separation -1)
          separation))

;;; When the window is lengthened, the better to reject frequencies far from
;;; the test frequency, the penalty is that the effect of a click in that
;;; frequency gets smeared over a wider interval in the convolution.  Try
;;; to compensate by reducing the detection thresholds.
     (compute-threshold-corrections
      (window bin-frequency bin-number step-size relative-threshold)
      (if (or (not use-threshold-corrections)
              (= bin-number 1))
          (cons 1.0 1.0)
          (let* ((srate (snd-srate window))
                 (sample-time (/ srate))
                 (bin-period (/ bin-frequency))
                 (n-samples (truncate (* bin-period srate)))
                 (step-dur (/ step-size srate))
                 (increase (- relative-threshold 1.0))
                 (period (/ bin-period bin-number))
                 (shift (* (- (/ bin-number 2.0) 0.75) period))
                 ;; define an envelope for the sinusoid, so that amplitude
                 ;; rises from 1 over a quarter period, to relative-threshold
                 ;; for one period in the middle, then drops to 1 again over
                 ;; another quarter period.  (But should I make the plateau
                 ;; wider then just one cycle?)
                 (pwl-list1 (list (round (* srate shift))
                                  0.0
                                  (round (* srate (+ shift (* period 0.25))))
                                  increase
                                  (round (* srate (+ shift (* period 1.25))))
                                  increase
                                  (round (* srate (+ shift (* period 1.5))))
                                  0.0
                                  (round (* srate bin-period))))
                 (env1 (snd-offset (snd-pwl 0.0 srate pwl-list1) 1.0))
                 ;; define a similar envelope but with the bump one block
                 ;; later than the middle of the interval.
                 (pwl-list2
                  (list (round (* srate (+ shift step-dur)))
                        0.0
                        (round (* srate (+ shift step-dur (* period 0.25))))
                        increase
                        (round (* srate (+ shift step-dur (* period 1.25))))
                        increase
                        (round (* srate (+ shift step-dur (* period 1.5))))
                        0.0
                        (round (* srate bin-period))))
                 (_ (do ((lis pwl-list2 (cddr lis)))
                        ((not lis))
                      (when (and (third lis)
                                 (>= (car lis) (* srate bin-period)))
                        (rplacd (cdr lis) (cons (+ sample-time (car lis)) nil))
                        (return))))
                 (env2 (snd-offset (snd-pwl 0.0 srate pwl-list2) 1.0))
                 ;; Multiply sinusoid by each envelope, and by window, and sum.
                 ;; The ratio of the sums tells us how prominent the click
                 ;; remains after a smoothing.
                 (cosines (cosine-periods
                           srate (* bin-frequency bin-number) bin-number))
                 (num (snd-fetch (snd-avg (prod env1 cosines window)
                                          n-samples n-samples OP-AVERAGE)))
                 (denom (snd-fetch (snd-avg (prod env2 cosines window)
                                            n-samples n-samples OP-AVERAGE))))
            (cons (/ (/ num denom) relative-threshold)
                  (/ (* num n-samples) relative-threshold)))))

     (initialize-buffer
      (sig padding max-steps)
      (let* ((size (+ max-steps padding padding))
             (array (make-array size)))
        (dotimes (ii size)
          (setf (aref array ii) (snd-fetch sig)))
        (cons array 0)))

     (advance-buffer
      (sig buffer steps)
      (let* ((array (car buffer))
             (len (length array)))
        (dotimes (ii steps)
          (setf (aref array (cdr buffer)) (snd-fetch sig)
                (cdr buffer) (rem (1+ (cdr buffer)) len)))))

     (fetch-buffer
      (buffer direction base offset)
      (aref (car buffer)
            (rem (+ (cdr buffer) base (* direction offset))
                 (length (car buffer)))))

     (test-click-boundary
      (direction index buffer amp-buffer
                 separation crackle-thresholds bar rise-time results)
      (let* ((padding (max separation rise-time))
             (offset padding)
             limit
             sample
             amp-sample
             (amp-bar nil)
             background)
        (dotimes (ii rise-time (return-from test-click-boundary nil))
          (when
              (and
               (setq offset (1- offset)
                     background (fetch-buffer buffer direction index offset))
               (< background bar))
            (return)))
        (setq limit (max 0 (- offset (- padding separation))))
        (setf
         (aref results 0)
         (dotimes (ii limit separation)
           (cond
            ((or
              (null (setq offset (1- offset)
                          sample (fetch-buffer buffer direction index offset)))
              (and
               (or amp-bar ;; edge found already
                   (and (>= sample bar) ;; edge found here
                        (setq amp-bar (aref crackle-thresholds ii))))
               (or
                (null
                 (setq amp-sample
                       (fetch-buffer amp-buffer direction index offset)))
                (>= amp-sample amp-bar))))
             (return (1- (- padding offset))))
            ((null amp-bar)
             (update-max background sample)))))
        background))

     (test-click
      (buffer amp-buffer separation crackle-thresholds
              relative-threshold absolute-threshold
              rise-time max-steps results)
      ;; always indicate length of click or 0
      (setf (aref results 2) 0)
      (let* ((new-separation (one-side-separation separation))
             (twice-separation (+ separation separation))
             (index (max rise-time new-separation))
             (foreground1
              (or (fetch-buffer buffer 1 index 0)
                  (throw 'done nil))) ; ultimate nil return from iterator
             (background1
              (or (test-click-boundary
                   1 0 buffer amp-buffer new-separation crackle-thresholds
                   (/ foreground1 relative-threshold) rise-time results)
                  (return-from test-click nil)))
             (length1 (aref results 0))
             background2
             foreground
             background
             width)
        (when (< length1 (- twice-separation new-separation))
          (return-from test-click nil))
        ;; start looks good; find end
        (do* ((n-steps 1 (1+ n-steps))
              (index2 index (1+ index2))
              (foreground2 foreground1 (fetch-buffer buffer 1 index2 0))
              min-foreground
              max-foreground)
            ((or (null foreground2) (> n-steps max-steps)))
          (update-max max-foreground foreground2)
          (update-min min-foreground foreground2)
          (cond
           ((and
             (setq background
                   (test-click-boundary
                    -1 (+ n-steps -1 (* 2 (max rise-time new-separation)))
                    buffer amp-buffer new-separation crackle-thresholds
                    (if stricter-test
                        (/ min-foreground relative-threshold)
                        (/ foreground2 relative-threshold))
                    rise-time results))
             (or (not stricter-test)
                 (> min-foreground
                    (* relative-threshold (max background1 background))))
             (>= (+ length1 (aref results 0)) twice-separation)
             (> min-foreground absolute-threshold))
            ;; set, or extend, the end-of-click
            (setq foreground max-foreground
                  background2 background
                  width n-steps))
           (width
            ;; done with extending some click
            (return))))
        (and width
             (setf (aref results 0) (/ foreground background1)
                   (aref results 1) (/ foreground background2)
                   (aref results 2) width))))

     ;; frequency argument is only passed through to detail constructor.
     (make-single-frequency-click-iterator
      (t0 sig amp-sig step-dur rise-time max-steps
          relative-threshold relative-threshold-correction
          absolute-threshold
          separation crackle-thresholds label-frequency)
      (let* ((corrected-threshold (* relative-threshold
                                     relative-threshold-correction))
             ;; multiply n-steps by step-dur and add t0, and you get the
             ;; corresponding time in the original sound.
             (n-steps (max rise-time (one-side-separation separation)))
             flag
             buffer
             amp-buffer
             (results (vector 0 0 0)))
        #'(lambda (limit-time)
            ;; this function may return t if the next interesting value is
            ;; at or after limit-time.
            (initially
             flag
             (setq buffer (initialize-buffer sig n-steps max-steps))
             (setq amp-buffer (initialize-buffer amp-sig n-steps max-steps)))
            (let ((limit-step (truncate (/ (- limit-time t0) step-dur))))
              (catch 'done ;; the nil return path
                (do (answer advance)
                    ((or answer (>= n-steps limit-step))
                     (or answer t))
                  (setq
                   answer
                   (and (test-click buffer amp-buffer
                                    separation
                                    crackle-thresholds
                                    corrected-threshold absolute-threshold
                                    rise-time
                                    max-steps results)
                        (click-detail
                         (+ t0 (* n-steps step-dur)) label-frequency
                         (aref results 2)
                         ;; undo the correction factor to record the gain
                         ;; really needed for repair
                         (/ (aref results 0) relative-threshold-correction)
                         (/ (aref results 1) relative-threshold-correction)))
                   advance (if answer (1+ (aref results 2)) 1)
                   n-steps (+ n-steps advance))
                  (advance-buffer sig buffer advance)
                  (advance-buffer amp-sig amp-buffer advance))))))))

;;; returns iterator that returns click-detail or nil or t
;;; The iterator takes a limiting time argument.  If no click is before that
;;; time but input remains, the result may be click-detail or just t.
;;; (This is done solely to improve progress indicator behavior.)
  (defun click-detail-finder (snd amp-sig band-bottom band-top
                                  separation crackle-thresholds
                                  step-size rise-time max-steps
                                  relative-threshold absolute-threshold)
    (let* ((srate (snd-srate snd))
           (step-dur (/ step-size srate))
           ;; isolate a band of frequencies
           (frequency (/ (+ band-bottom band-top) 2.0))
           (label-frequency (geometric-mean band-bottom band-top))
           ;; The detection uses the arithmetic mean of band bottom and top,
           ;; though the repair uses geometric.
           (width (- band-top band-bottom))
           (bin-number
            (if use-large-windows
                (let* ( ;; longer window narrows the main lobe of response.
                       ;; How narrow can we make it?
                       ;; Make the 3dB width not less than the width of the
                       ;; band.
                       ;; That leaves some overlap in successive main lobes,
                       ;; even when the bands are not widened,
                       ;; so our tests will cover the intended part of the
                       ;; spectrum.
                       (data
                        (cdr (assoc use-window-choice window-function-list)))
                       (minimum-bin
                        (/ width
                           (window-function-3dB-width data))))
                  ;; number of periods must be whole; lesser means a shorter
                  ;; window and a wider bin; therefore truncate down
                  ;; if we can.
                  (max 1 (truncate (/ frequency minimum-bin))))
                1))
           (bin-frequency (/ frequency bin-number))
           (unnormalized-window
            (if use-double-convolution
                (compute-double-convolution-window srate band-bottom band-top)
                (compute-convolution-window srate bin-frequency bin-number)))
           (window
            (normalize-convolution-window unnormalized-window frequency))
           (corrections (compute-threshold-corrections
                         window bin-frequency bin-number step-size
                         relative-threshold))
           (relative-threshold-correction (car corrections))
           (absolute-threshold-correction (cdr corrections))
           (_ (when debug-messages
                (print (format nil
                               "Correct rel. thresholds by ~A and abs. by ~A"
                               relative-threshold-correction
                               absolute-threshold-correction))))
           (_ (when debug-convolution-window
                (throw 'debug-curve window)))
           (window-size (snd-length window ny:all))
           (window-dur (/ window-size srate))
           ;; Convolve snd with window, working around the crash bug
           ;; in Nyquist.  That is, avoid convolving with a window that
           ;; is "too long" by convolving in pieces instead.  I am not
           ;; quite sure how to define "too long," this seems to work
           ;; but might be too cautious.
           (n-pieces (if use-crash-workaround
                         (1+ (truncate (/ window-size step-size))) 1))
           (_ (when (and debug-messages (> n-pieces 1))
                (print (format
                        nil "window pieces for band at ~A Hz: ~A"
                        band-bottom n-pieces))))
           ;; Make this integer more than 1 to go faster at the expense of
           ;; some accuracy.  Don't make it so big that you alias the
           ;; test frequency!  I suppose I should avoid aliasing the
           ;; consine components in the window too.
           ;; Haven't tried this yet.
           (downsample-factor 1)
           ;;(max 1 (truncate (/ (/ srate) (+ band-bottom band-top)))))
           (conv1
            (if (> downsample-factor 1)
                ;; don't convolve the sounds directly, to
                ;; gain some performance at the expense of some accuracy
                ;; dividing time roughly by the square of the factor
                (let ((lesser-srate (/ srate downsample-factor)))
                  (prod downsample-factor
                        (force-srate 
                         srate (convolve-by-pieces
                                (snd-avg snd downsample-factor
                                         downsample-factor OP-AVERAGE)
                                (snd-avg window downsample-factor
                                         downsample-factor OP-AVERAGE)
                                        ;(resample snd lesser-srate)
                                        ;(resample window lesser-srate)
                                n-pieces))))
                (convolve-by-pieces snd window n-pieces)))
           ;; Yes, do integer division of window-size below.
           ;; I get burned sometimes if I forget to quantize sounds to
           ;; the sample time...
           (half-window-dur (/ (/ window-size 2) srate))
           ;; discard some initial samples, so that the first remaining sample
           ;; centers the window at the first sample of snd; that should
           ;; leave the phase of a sinusoid of frequency Hz also unchanged.
           (convolution (snd-xform conv1
                                   (snd-srate conv1) (snd-t0 conv1)
                                   (+ (snd-t0 conv1) half-window-dur)
                                   MAX-STOP-TIME 1.0))
           (_ (when debug-convolution
                (throw 'debug-curve convolution)))
           ;; find peak amplitude per block
           (sig (sum (db-to-linear -100) ;guard against divisions by zero
                     (snd-avg convolution step-size step-size OP-PEAK)))
           (_ (when debug-peaks
                (throw 'debug-curve (force-srate srate sig))))
           (corrected-crackle-thresholds
            (make-array (length crackle-thresholds))))
      (dotimes (ii (length crackle-thresholds))
        (setf (aref corrected-crackle-thresholds ii)
              (* absolute-threshold-correction (aref crackle-thresholds ii))))
      (make-single-frequency-click-iterator
       (snd-t0 sig) sig amp-sig step-dur rise-time max-steps
       relative-threshold relative-threshold-correction
       (* absolute-threshold absolute-threshold-correction)
       separation corrected-crackle-thresholds label-frequency))))

(flet
    ((consume-dummies
      (iter-arr lookaheads limit)
      (dotimes (ii (length lookaheads))
        (let ((val (aref lookaheads ii)))
          (if (and val (atom val))
              (setf (aref lookaheads ii) 
                    (funcall (aref iter-arr ii) limit)))))))

;;; First argument is array of iterators, each for a different frequency.
;;; The iterators return increasing start times.
;;; The output returns nondecreasing start times, with a minor sort according
;;; to the input array of frequencies.  Destroys iter-arr.
  (defun interleave-click-detail-finders (iter-arr t0 step-dur limit-time-step)
    (let* ((nn (length iter-arr))
           (lookaheads (make-array nn))
           (limit-time (+ t0 limit-time-step))
           least
           (position nn)
           flag)
      #'(lambda ()
          (initially flag
                     (dotimes (ii nn)
                       (setf (aref lookaheads ii)
                             (funcall (aref iter-arr ii) limit-time))))
          (loop
           ;; If the "least" time in the array is known, emit all values
           ;; for that time, before finding the next time.
           (let (val
                 (answer (do ()
                             ((= position nn) nil)
                           (setq val (aref lookaheads position))
                           (cond ((and val (not (atom val))
                                       (= (click-detail-time val) least))
                                  (setf (aref lookaheads position)
                                        (funcall (aref iter-arr position)
                                                 limit-time))
                                  (setq positition (1+ position))
                                  (return val))
                                 (t (incf position))))))
             (when answer (return answer))
             ;; try to find the least time, but don't advance the iterators too
             ;; unequally in time, so that the progress indicator behaves
             ;; better.
             (setf least nil)
             (let (more)
               (dotimes (ii nn)
                 (let ((val (aref lookaheads ii)))
                   (cond ((not val))
                         ((atom val) (setf more t))
                         (t (update-min least (click-detail-time val))))))
               (cond
                ((and least (< least limit-time))
                 ;; some time in the array is less than the limit.
                 (setf position 0))
                (more
                 ;; some times in the array are unknown, therefore at
                 ;; or after the limit.  Try to define them before determining
                 ;; the next least time.  The least, defined time may not
                 ;; be correct to emit yet.
                 (consume-dummies iter-arr lookaheads
                                  (add-to limit-time limit-time-step)))
                (least
                 ;; All times are known, and some are not nil,
                 ;; so advance the limit to more than the least but less
                 ;; than the next greater possible time value, and repeat.
                 (setf limit-time (+ least (/ step-dur 2))  position 0))
                (t (return nil))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find clicks in ranges of frequencies

;;; constructor and accessors
(defmacro click-info-start (info) ; global time
  `(aref (car ,info) 0))
(defmacro click-info-steps (info) ; integer to multiply by step duration
  `(aref (car ,info) 1))
(defmacro click-info-low (info) ; Hz
  `(aref (car ,info) 2))
(defmacro click-info-high (info) ; Hz
  `(aref (car ,info) 3))
(defmacro click-info-details (info) ; list of click-detail
  `(aref (car ,info) 4))
(defmacro click-info-extra (info) ; anything
  `(aref (car ,info) 5))

(setf click-info-free-list (init-free-list (cons (make-array 6) nil)))
(defun click-info (start steps low high details &optional extra)
  (let ((cell (pop-free-list click-info-free-list)))
    (if cell
        (let ((v (car cell)))
          (setf (aref v 0) start
                (aref v 1) steps
                (aref v 2) low
                (aref v 3) high
                (aref v 4) details
                (aref v 5) extra)
          cell)
        (cons (vector start steps low high details extra) nil))))
(defun free-click-info (click)
  (do ((click1 click (cdr click1)))
      ((not click1))
    (free-click-detail (click-info-details click1))
    (if (not (atom (click-info-extra click1)))
        (free-click-detail (click-info-extra click1))))
  (push-free-list click click-info-free-list))

(defun click-details-max-ratio (details results) ; put linear ratio and
                                        ;frequency in results
  (let (ratio freq)
    (do ((detail details (cdr detail)))
        ((not detail) (setf (car results) ratio (cdr results) freq))
      (let ((detail-ratio (click-detail-ratio detail)))
        (if (or (not ratio) (> detail-ratio ratio))
            (setf ratio detail-ratio 
                  freq (click-detail-frequency detail)))))
    results))

(defun click-details-max-gain (details results) ; returns dB
  (click-details-max-ratio details results)
  (setf (car results) (linear-to-db (car results)))
  results)

(defun click-info-max-gain (info results) ; returns dB
  (click-details-max-gain (click-info-details info) results))

;;; Given an array of iterators returning click-detail at nondecreasing
;;; start times.
;;; Return an iterator returning combined click information for
;;; nondecreasing start times (increasing if combine-unequal-lengths is true).
;;; Only combines details for the same starting time.
;;; Does not combine all overlapping cases.
(defun combine-click-details
  (iter relative-threshold tag combine-unequal-lengths)
  (let ((pair (cons nil nil))
        flag lookahead)
    #'(lambda ()
        (initially flag (setf lookahead (funcall iter)))
        (loop
         (cond (lookahead
                (let* ((time (click-detail-time lookahead))
                       (steps (click-detail-steps lookahead))
                       (low (click-detail-frequency lookahead))
                       (high low)
                       (details
                        (do1-link-list
                            lookahead
                            ((_ (setf lookahead (funcall iter))))
                            (not (and lookahead
                                      (= time (click-detail-time lookahead))
                                      (or combine-unequal-lengths
                                          (= steps (click-detail-steps
                                                    lookahead)))))
                          (update-min low (click-detail-frequency lookahead))
                          (update-max high (click-detail-frequency lookahead))
                          (update-max steps (click-detail-steps lookahead))
                          lookahead)))
                  (if (or (not relative-threshold)
                          ;; STEEP-ENOUGH test, late
                          ;; We have eliminated some tiny click details already
                          ;; but others that are by theselves below the
                          ;; relative threshold may survive in combination with
                          ;; a simultaneous click in another band that is above
                          ;; threshold.
                          ;; That lets us add some finer detail to the
                          ;; equalization curve for repair later.
                          (>= (car (click-details-max-ratio details pair))
                              relative-threshold))
                      (return (click-info time steps low high details tag))
                      (free-click-detail details))))
               (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Takes an iterator returning click-info with nondecreasing start times,
;;; returns an iterator returning lists of click-info,
;;; grouping clicks that overlap in time, when their intervals are extended
;;; at each end by the amount extend-interval.
;;; (i.e. each group is a class of the equivalence closure of the overlapping
;;; relation.)
(defun group-overlapping-clicks (click-iterator step-dur extend-interval)
  (let (flag lookahead)
    #'(lambda ()
        (initially flag (setf lookahead (funcall click-iterator)))
        (and lookahead
             (let* ((extend-start
                     (- (click-info-start lookahead) extend-interval))
                    (extend-end (+ extend-start
                                   (* step-dur (click-info-steps lookahead))
                                   extend-interval extend-interval)))
               (do1-link-list
                   lookahead
                   ((_ (setf lookahead (funcall click-iterator)))
                    (next-start (and lookahead
                                     (click-info-start lookahead))))
                   (or (not next-start)
                       ;; < instead?
                       (<= extend-end (- next-start extend-interval)))
                 (update-max extend-end
                             (+ next-start
                                (* step-dur
                                   (click-info-steps lookahead))
                                extend-interval))
                 lookahead))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; returns iterator returning click-info for nondecreasing start times
;;; (increasing if combine-unequal-lengths is true)
;;; limit-time-step argument affects progress indicator smoothness only
(defun make-click-iterator (snd frequency-list
                                separation crackle-thresholds
                                step-size rise-time max-steps
                                relative-threshold absolute-threshold
                                tag
                                &optional
                                (limit-time-step (- MAX-STOP-TIME (snd-t0 snd)))
                                (combine-unequal-lengths t))
  (when (or debug-convolution-window
            debug-convolution
            debug-peaks)
    (dotimes (ii debug-band)
      (pop frequency-list)))
  (let* ((amp-sig
;;          (highpass8
           (snd-avg snd step-size step-size OP-PEAK)
  ;;         60.0)
          )
         (click-detail-finders
          (apply #'vector
                 (do-build-list
                     ((bottom (pop frequency-list) top)
                      (top (pop frequency-list) (pop frequency-list)))
                     (not top)
                   (click-detail-finder
                    snd (snd-copy amp-sig)
                    (if use-wide-detection-bands
                        (/ bottom (sqrt (/ top bottom)))
                        bottom)
                    (if use-wide-detection-bands
                        (* top (sqrt (/ top bottom)))
                        top)
                    separation crackle-thresholds step-size rise-time max-steps
                    (if use-late-steepness-test
                        ;; Filter out only the click-details that are
                        ;; just "noise."  But how to define that threshold?
                        ;; Square root of the given threshold (halving in
                        ;; dB terms) for now.
                        (sqrt relative-threshold)
                        relative-threshold)
                    absolute-threshold))))
         (interleaved-finder
          (interleave-click-detail-finders
           click-detail-finders (snd-t0 snd) (/ step-size (snd-srate snd))
           limit-time-step)))
    (combine-click-details
     interleaved-finder (if use-late-steepness-test relative-threshold nil)
     tag combine-unequal-lengths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convert click-info to labels
;;; If the extra slot of a click is not an atom, assume it is another
;;; list of details for the right channel, with the same times and
;;; high and low frequencies

(defun make-labels (click-iterator step-dur long-form)
  (let ((pair (cons nil nil)))
    (do1-build-list
        ((click
          ;;(progv '(*gc-flag*) '(t)
          (funcall click-iterator)
          ;;          )
          ))
        (not click)
      (progv
       '(*float-format*)
       '("%5.1f")
       (let* ((time (click-info-start click))
              (steps (click-info-steps click))
              (low-freq (click-info-low click))
              (high-freq (click-info-high click))
              (pair1 (click-info-max-gain click pair))
              (gain1 (car pair1))
              (freq1 (cdr pair1))
              (tag (click-info-extra click))
              (pair2 (and (not (atom tag)) (click-details-max-gain tag pair)))
              (gain2 (car pair2))
              (freq2 (cdr pair2))
              (prefix-string (cond ((not (atom tag)) "L|R:")
                                   ((eq tag 'L) "L:")
                                   ((eq tag 'R) "R:")
                                   (t "")))
              (db-string (if (and gain2 (not (= gain1 gain2)))
                             (format nil "~A dB at ~A Hz | ~A dB at ~A Hz"
                                     gain1 freq1 gain2 freq2)
                             (format nil "~A dB at ~A Hz" gain1 freq1)))
              (freq-string (if (and long-form (/= low-freq high-freq))
                               (format nil
                                       " (~A--~A Hz)" low-freq high-freq)
                               ""))
              (result-string
               (format nil "~A~A~A" prefix-string db-string freq-string)))
         (free-click-info click)
         (list time (+ time (* steps step-dur)) result-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this avoids some repeated allocations

(defun envelope-factory (init-count f step-dur crossfade-interval)
  (let* ((count init-count)
         (arr (make-array count)))
    #'(lambda (start-time step-multiple)
        (when (> step-multiple count)
          ;; grow
          (let* ((new-count (max step-multiple (* 2 count))) 
                 (new-arr (make-array new-count)))
            (dotimes (ii count) (setf (aref new-arr ii) (aref arr ii)))
            (setf arr new-arr  count new-count)))
        (let ((ii (1- step-multiple)))
          (when (not (aref arr ii))
            (let ((dur (+ (* 2 crossfade-interval)
                          (* step-dur step-multiple))))
              (setf (aref arr ii) (funcall f dur))))
          (shift-time (aref arr ii) start-time)))))

;;; make smooth control envelopes of various widths
(defun raised-cosine-function (srate)
  #'(lambda (dur)
      (prod 0.5 (diff 1 (cosine-periods srate (/ dur) 1)))))

;;; make piecewise linear envelopes of various widths
(defun double-ramp-function (srate crossfade-interval)
  #'(lambda (dur)
      (let* ((total (truncate (* dur srate)))
             (partial (round (* total (/ crossfade-interval dur)))))
        (when (> (+ partial partial) total)
          (error "ill defined ramp function"))
        (snd-pwl 0.0 srate (list partial 1.0 (- total partial) 1.0 total)))))

;;; make constant unit envelopes of various widths
(defun unit-function (srate)
  #'(lambda (dur) (snd-const 1.0 0.0 srate dur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Repair of single clicks

;;; Fix a click by using eq-band iteratively,
;;; centered on each noisy frequency.
(defun eq-bands-click-fixer
  (repair-threshold frequency-list
                    max-steps step-dur srate
                    crossfade-interval variable-gains)
  (let* ( ;; These factories are needed only for variable gains.
         ;; Is there any reason to prefer one over the other envelope for
         ;; the gains?
         ;;(rc-func (raised-cosine-function srate))
         (dr-func (double-ramp-function srate crossfade-interval))
         (gain-envs (envelope-factory max-steps dr-func
                                      step-dur crossfade-interval))
         ;; Must make constant envelopes for frequency and widths just so that
         ;; we can use the varying one for gains, because that is what
         ;; eq-band demands.
         (unit-func (unit-function srate))
         (constant-envs (envelope-factory max-steps unit-func
                                          step-dur crossfade-interval)))
    #'(lambda (click excerpt)
        (let* ((my-frequency-list frequency-list)
               (t0 (snd-t0 excerpt))
               (result excerpt)
               (steps (click-info-steps click))
               (detail (click-info-details click))
               (detail-frequency (click-detail-frequency detail)))
          ;; assume details and frequency-list go by increasing frequency
          (do ((prev-frequency (pop my-frequency-list) next-frequency)
               (next-frequency (pop my-frequency-list) (pop my-frequency-list))
               center width gain)
              ((not detail) result)
            (when (not (and prev-frequency next-frequency
                            (< prev-frequency next-frequency)))
              (error "bad frequency list"))
            (when (and (<= prev-frequency detail-frequency)
                       (< detail-frequency next-frequency))
              ;; The detection used the arithmetic mean of
              ;; band bottom and top,
              ;; though the repair will use geometric.
              (setq 
               center (geometric-mean prev-frequency next-frequency)
               ;; half gain width in octaves
               ;; if the half-gain width about the center of each
               ;; of (logarithmically) equal sized bands is at the
               ;; boundaries of the band, then when we compose effects,
               ;; the gain in dB versus log of
               ;; frequency should be an approximate piecewise linear
               ;; curve, bending at the centers.
               width (/ (log (/ next-frequency prev-frequency)) LOG2)
               gain (- repair-threshold
                       (click-detail-difference detail)))
              (when (> gain 0.0)
                (print "positive gain!")
                ;(error "positive gain")
                (setq gain 0.0)
                )
              (setf result
                    (if variable-gains
                        (let* ((gain-env
                                (prod gain (funcall gain-envs t0 steps)))
                               (const-env (funcall constant-envs t0 steps))
                               (const-freq (prod center const-env))
                               (const-width (prod width const-env)))
                          (eq-band result const-freq gain-env const-width))
                        (eq-band result center gain width)))
              (setf detail (cdr detail)
                    detail-frequency
                    (and detail (click-detail-frequency detail)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Repair of multiple clicks

(defun compute-overlapping-differences
  (snd click-fixer overlap-list step-size
       crossfade-interval crossfade-envs)
  (let* ((srate (snd-srate snd))
         (step-dur (/ step-size srate))
         range-diff)
    (if (null overlap-list) (error "null overlap-list"))
    ;; range-diff will be non-null at the end.
    ;; My computer took 1:44 to process an 8 minute selection of speech.
    ;; Flattening the answers before they accumulate improves
    ;; performance greatly.  Without that, processing time was 1:27:18 !
    (do ((click overlap-list (cdr click)))
        ((not click) (prog1 range-diff
                       (snd-flatten range-diff ny:all)))
      ;; the max 0 guard is an unlikely case
      ;; but I should handle it better
      (let* ((fix-start (max 0 (- (click-info-start click) crossfade-interval)))
             (steps (click-info-steps click))
             (fix-dur (+ (* steps step-dur) (* 2 crossfade-interval)))
             (fix-end (+ fix-start fix-dur))
             ;; We must add prior overlapping fix differences to get
             ;; the correct excerpt.  Testing whether there are any,
             ;; rather than adding all previous differences and trimming,
             ;; is another significant optimization.  This is a benefit of
             ;; determining the overlaps first, though it is not needed to
             ;; calculate correct results.  Without this, processing time
             ;; increases to 3:49.
             (temp-snd (if range-diff (sum range-diff snd) snd))
             (excerpt (trim-snd temp-snd fix-start fix-dur))
             (fix (funcall click-fixer click excerpt))
             (difference (diff fix excerpt))
             (crossfade-env (funcall crossfade-envs fix-start steps))
             (faded-difference (prod crossfade-env difference)))
        ;; apparently significant additional improvement too sometimes:
        (snd-flatten faded-difference ny:all)
        (setf range-diff
              (if range-diff (sum range-diff faded-difference)
                  faded-difference))))))

(defun overlap-difference-iterator (snd group-iter click-fixer
                                        max-steps step-size
                                        crossfade-interval)
  (let* ((srate (snd-srate snd))
         (step-dur (/ step-size srate))
         (dr-func (double-ramp-function srate crossfade-interval))
         (crossfade-envs (envelope-factory max-steps dr-func
                                           step-dur crossfade-interval)))
    #'(lambda ()
        (let ((overlap-list (funcall group-iter)))
          (and overlap-list
               (prog1
                   (compute-overlapping-differences
                    snd click-fixer overlap-list
                    step-size
                    crossfade-interval crossfade-envs)
                 (free-click-info overlap-list)))))))

(macrolet
;;; abstracting different ways for building up a sound
;;; note that t0 is a "free variable"
    ((init-sound () `nil)
     (accumulate-sound (snd term)
                       (let ((val (gensym)))
                         `(let ((,val ,term))
                            (setf ,snd (if (not ,snd) ,val (sum ,snd ,val))))))
     (result-sound (snd)
                   `(or ,snd (snd-const 0.0 t0 srate 0.0)))

     ;; (init-sound () `(snd-const 0.0 t0 srate 0.0))
     ;; (accumulate-sound (snd term) `(setf ,snd (sum ,snd ,term)))
     ;; (result-sound (snd) snd)
     )

#|
  (defun compute-differences (snd click-iterator click-fixer
                                   max-steps step-size
                                   crossfade-interval)
    (let* ((t0 (snd-t0 snd))
           (srate (snd-srate snd))
           (step-dur (/ step-size srate))
           (result (init-sound))
           (group-iter (group-overlapping-clicks
                        click-iterator step-dur crossfade-interval))
           (overlap-iter (overlap-difference-iterator 
                          snd group-iter click-fixer max-steps step-size
                          crossfade-interval)))
      (do1 ((overlap-result (funcall overlap-iter)))
          ((not overlap-result) (result-sound result))
        (accumulate-sound result overlap-result))))
|#

  ;; This version with snd-seq give smoother overall progress indicator
  ;; by interleaving the generation of the result and the reading of the
  ;; original.  It helps performance and intermediate memory usage.
  (defun compute-differences (snd click-iterator click-fixer
                                  max-steps step-size
                                  crossfade-interval)
    (let* ((t0 (snd-t0 snd))
           (srate (snd-srate snd))
           (step-dur (/ step-size srate))
           (group-iter (group-overlapping-clicks
                        click-iterator step-dur crossfade-interval))
           (overlap-iter (overlap-difference-iterator 
                          snd group-iter click-fixer max-steps step-size
                          crossfade-interval)))
      (labels
          ((compute-piece
            (time)
            (let ((overlap-result (funcall overlap-iter)))
              (if overlap-result
                  (snd-seq overlap-result #'compute-piece)
                  (snd-zero time srate)))))
        (compute-piece t0)))))

 ;;; Find an increasing list of n+1 frequencies defining n bands.
(defun find-frequencies
  (frequency-bound1 frequency-bound2 n-bands band-type)
  (let* ((floor (min frequency-bound1 frequency-bound2))
         (ceiling (max frequency-bound1 frequency-bound2))
         (difference (- ceiling floor))
         (increment (/ difference n-bands))
         (multiplier (case band-type
                           ;; logarithmic
                           (0 (exp (/ (log (/ ceiling floor)) n-bands)))
                           ;; linear
                           (t nil))))
    (do-build-list
        ;; n bands are defined by n+1 frequencies, so start ii at -1.
        ((ii -1 (1+ ii))
         (frequency floor (if multiplier
                              (* frequency multiplier)
                              (+ frequency increment))))
        (= ii n-bands)
      frequency)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main routines use the global variables

;;; Find click information, return it if only labelling, else compute
;;; new sound.
(defun make-channel-results (snd0 frequency-list
                                  step-size rise-time max-steps tag)
  (let* ((t0 (snd-t0 snd0))
         (srate (snd-srate snd0))
         (differences (snd-zero t0 srate))
         (crackle-thresholds
          (compute-crackle-thresholds
           control-crackle-threshold control-separation rise-time))
         (times (max 1 control-passes))
         stagger
         (stagger-time 0.0)
         (snd snd0)
         (labels-only (> control-action-choice 1))
         (crossfade-interval
          ;; be careful to quantize to sample time!
          (and (not labels-only)
               (/ (round
                   (* (/ control-crossfade-interval 1000.0) srate))
                  srate)))
         (click-fixer
          (and (not labels-only)
               (let ((step-dur (/ step-size srate))                     
                     (repair-threshold
                      (- (case control-fix-type
                               (0 0.0)
                               (1 control-relative-threshold))
                         (if debug-overshoot
                             (* 0.5 control-relative-threshold) 0.0))))
                 (eq-bands-click-fixer
                  repair-threshold
                  frequency-list max-steps step-dur
                  srate crossfade-interval nil)))))
    (catch 'debug-curve
      (dotimes
          (ii times
              (case control-action-choice
                    (0 ;; add a silence coterminous with snd.
                     ;; Delay call of snd-length to the end
                     ;; so the progress indicator behaves nicely, not rushing
                     ;; through and then pausing.
                     (sum (snd-const 0.0 t0 srate
                                     (/ (snd-length snd0 ny:all)
                                        srate))
                          differences))
                    (1 (sum snd0 differences))))
        (when (> ii 0)
          (setq snd (sum snd0 differences))
          (snd-flatten snd ny:all)
          (when use-stagger
            ;; make the step boundaries fall a little differently
            ;; in each later pass, maybe that will catch more marginal cases?
            (setq stagger (/ (* step-size ii) times)
                  stagger-time (/ stagger srate)
                  snd (snd-xform snd srate t0
                                 (+ t0 stagger-time) MAX-STOP-TIME 1.0))))
        (let ((click-iterator (make-click-iterator
                               snd frequency-list
                               control-separation crackle-thresholds
                               step-size rise-time max-steps
                               (db-to-linear control-relative-threshold)
                               (db-to-linear control-absolute-threshold)
                               tag
                               (/ (get-duration 1) 100.0))))
          (when labels-only (return click-iterator))
          (setq differences
                (sum differences
                     (shift-time (compute-differences
                                  snd click-iterator click-fixer
                                  max-steps step-size
                                  crossfade-interval)
                                 stagger-time))))))))

(defun make-mono-results (snd frequency-list step-size rise-time max-steps)
  (let ((channel-results
         (make-channel-results snd frequency-list
                               step-size rise-time max-steps nil)))
    (if (soundp channel-results)
        channel-results
        (make-labels channel-results (/ step-size (snd-srate snd))
                     (= control-action-choice 3)))))

(labels 
    ((compare (num1 num2)
              (cond
               ((< num1 num2) -1)
               ((< num2 num1) 1)
               (t nil)))

     ;; compare start time and low frequency before the others, to
     ;; agree with the sort order of the two streams.  Then compare
     ;; the rest, when deciding whether to merge.
     (compare-clicks
      (click1 click2)
      (or
       (compare (click-info-start click1) (click-info-start click2))
       (compare (click-info-low click1) (click-info-low click2))
       (compare (click-info-steps click1) (click-info-steps click2))
       (compare (click-info-high click1) (click-info-high click2))
       0))

;;; Take clicks from two channels, and wherever two have the same times
;;; and frequencies (but maybe not the same in other info), merge them.
     (merge-stereo-results
      (iter1 iter2)
      (let (flag click1 click2)
        #'(lambda ()
            (initially flag (setf click1 (funcall iter1) 
                                  click2 (funcall iter2)))
            (and (or click1 click2)
                 (let ((choice (cond ((not click2) -1)
                                     ((not click1) 1)
                                     (t (compare-clicks click1 click2)))))
                   (case choice
                         (-1 (prog1 click1 (setf click1 (funcall iter1))))
                         (1 (prog1 click2 (setf click2 (funcall iter2))))
                         (0 (prog1
                                ;; construct a new click with right channel
                                ;; details in the extra slot
                                (click-info
                                 (click-info-start click1)
                                 (click-info-steps click1)
                                 (click-info-low click1)
                                 (click-info-high click1)
                                 (click-info-details click1)
                                 (click-info-details click2))
                              (setf click1 (funcall iter1)
                                    click2 (funcall iter2)))))))))))

  (defun make-stereo-results (snds frequency-list
                                   step-size rise-time max-steps)
    (let ((channel-results-L (make-channel-results
                              (aref snds 0) frequency-list
                              step-size rise-time max-steps 'L))
          (channel-results-R (make-channel-results
                              (aref snds 1) frequency-list
                              step-size rise-time max-steps 'R)))
      (if (soundp channel-results-L)
          (vector channel-results-L
                  ;; sim 0 to work round bug 425
                  (sim 0 channel-results-R))
          (make-labels
           (merge-stereo-results channel-results-L channel-results-R)
           (/ step-size (snd-srate (aref snds 0)))
           (= control-action-choice 3))))))

 (cond
  ((and use-large-windows
        (/= control-band-type 0))
   "Large windows not recommended for linear frequency step")
  ;; Forgive if the first is greater than the second, but not if equal
  ((and (> control-action-choice 1)
        (> control-passes 1))
   "Labels available only for one pass")
  ((= control-frequency-bound1 control-frequency-bound2)
   "Please specify a nonempty frequency range")
  (t
   (let* ((frequency-list
           (find-frequencies control-frequency-bound1 control-frequency-bound2
                             control-n-bands control-band-type))
          (srate (snd-srate (if (arrayp s) (aref s 0) s)))
          (step-size (truncate (* srate (/ control-step-dur-ms 1000.0)))))
     ;; deal with mono or stereo inputs
     (or
      (if (arrayp s)
          (make-stereo-results s frequency-list
                               step-size control-rise-time control-max-steps)
          (make-mono-results s frequency-list
                             step-size control-rise-time control-max-steps))
      "Found no labels"))))
