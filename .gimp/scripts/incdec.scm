(define size-table '())


(define (change-brush-radius-relative-old-style scale)
  (let* ((brush (car (gimp-context-get-brush)))
         (radius (car (gimp-brush-get-radius brush)))
         (new-radius (* radius scale)))
    (gimp-brush-set-radius brush new-radius)
    (gimp-context-set-brush-size (max (* new-radius 2) 1))))

(define (script-fu-increase-brush-radius-relative-old-style)
  (change-brush-radius-relative-old-style 1.1))

(define (script-fu-decrease-brush-radius-relative-old-style)
  (change-brush-radius-relative-old-style (/ 1 1.1)))

(define (script-fu-reset-tool-size)
  (change-brush-radius-relative-old-style 1))

(script-fu-register
 "script-fu-increase-brush-radius-relative-old-style"
 "Increase brush radius(old style)"
 "Increase brush size and reset scale"
 "dechimal"
 "dechimal"
 "2012/9/27"
 "")

(script-fu-menu-register
 "script-fu-increase-brush-radius-relative-old-style"
 "<Image>/Edit")

(script-fu-register
 "script-fu-decrease-brush-radius-relative-old-style"
 "Decrease brush radius(old style)"
 "Decrease brush size and reset scale"
 "dechimal"
 "dechimal"
 "2012/9/27"
 "")

(script-fu-menu-register
 "script-fu-decrease-brush-radius-relative-old-style"
 "<Image>/Edit")

(script-fu-register
 "script-fu-reset-tool-size"
 "Reset tool size with current brush radius"
 "Reset tool size with current brush radius"
 "dechimal"
 "dechimal"
 "2012/9/28"
 "")

(script-fu-menu-register
 "script-fu-reset-tool-size"
 "<Image>/Edit")

