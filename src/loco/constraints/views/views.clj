;; http://www.choco-solver.org/apidocs/org/chocosolver/solver/variables/IViewFactory.html
;; views look like an optimization for some 1 arity arithmetic use
;; cases there are some constraints on some views that are not the
;; same as their intuitive constraint counterparts.  views produce a
;; variable, but unlike the variable generators, views don't allow for
;; names.

;; default BoolVar 	boolNotView(BoolVar bool)
;; Creates a view over bool holding the logical negation of bool (ie, ¬BOOL).
;; default IntVar 	intAbsView(IntVar var)
;; Creates a view over var such that: |var|.
;; default IntVar 	intMinusView(IntVar var)
;; Creates a view over var equal to -var.
;; default IntVar 	intOffsetView(IntVar var, int cste)
;; Creates a view based on var, equal to var+cste.
;; default IntVar 	intScaleView(IntVar var, int cste)
;; Creates a view over var equal to var*cste.
;; default RealVar 	realIntView(IntVar var, double precision)
;; Creates a real view of var, i.e. a RealVar of domain equal to the domain of var.
;; default RealVar[] 	realIntViewArray(IntVar[] ints, double precision)
;; Creates an array of real views for a set of integer variables This should be used to include an integer variable in an expression/constraint requiring RealVar
;; default RealVar[][] 	realIntViewMatrix(IntVar[][] ints, double precision)
;; Creates a matrix of real views for a matrix of integer variables This should be used to include an integer variable in an expression/constraint requiring RealVar

;;TODO: write scale partial resolver and tests
#_(defn scale
  "(scale :i_scale_2 :i 2) or ($= 4 (scale :i 2))"
  ([label dependency magnitude]
   {:pre [(keyword? label) (keyword? dependency) (integer? magnitude)]}
   ^{:scale dependency :magnitude magnitude} [:var label :proto])
  ([dependency magnitude]
   {:pre [(keyword? dependency) (integer? magnitude)]}
   [:constraint :partial [:scale dependency magnitude]]))

;;TODO: write offset partial resolver and tests
#_(defn offset
  "(offset :i_offset_2 :i 2) or ($= 4 (offset :i 2))"
  ([label dependency magnitude]
   {:pre [(keyword? label) (keyword? dependency) (integer? magnitude)]}
   ^{:offset dependency :magnitude magnitude} [:var label :proto])
  ([dependency magnitude]
   {:pre [(keyword? dependency) (integer? magnitude)]}
   [:constraint :partial [:offset dependency magnitude]]))
