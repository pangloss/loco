;; (ns loco.constraints.or)

;; ;; handle boolean lists
;; [:or (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
;; (.or model (->> bools (map lookup-var) (into-array BoolVar)))

;; [:or (constraints :guard (p every? model/constraint?))]
;; (.or model (realize-nested-constraints constraints))
