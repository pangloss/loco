(ns loco.constraints.and)

;; ;; -------------------- LOGIC --------------------
;; ;; handle boolean lists
;; [:and (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
;; (.and model (->> bools (map lookup-var) (into-array BoolVar)))

;; [:and (constraints :guard (p every? model/constraint?))]
;; (.and model (realize-nested-constraints constraints))
