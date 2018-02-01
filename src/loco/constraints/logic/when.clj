(in-ns 'loco.constraints)
;; (ns loco.constraints.when)


;; [:when [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked)) then-constraint]]
;; (.ifThen model
;;          (lookup-var bool)
;;          (realize-nested-constraint then-constraint))

;; [:when [if-constraint then-constraint]]
;; (.ifThen model
;;          (realize-nested-constraint if-constraint)
;;          (realize-nested-constraint then-constraint))
