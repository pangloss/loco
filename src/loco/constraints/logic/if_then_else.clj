(ns loco.constraints.if-then-else)


;; [:if-else [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked))
;;            then-constraint else-constraint]]
;; (.ifThenElse model
;;              (lookup-var bool)
;;              (realize-nested-constraint then-constraint)
;;              (realize-nested-constraint else-constraint))

;; [:if-else [if-constraint then-constraint else-constraint]]
;; (.ifThenElse model
;;              (realize-nested-constraint if-constraint)
;;              (realize-nested-constraint then-constraint)
;;              (realize-nested-constraint else-constraint))
