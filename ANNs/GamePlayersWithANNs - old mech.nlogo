;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Players who use Artifical Neural Networks
;; (C) Christopher J Watts, 2025.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [
  sorted-people

  selected-an-node

  recent-cost
  sorted-people-by-mfi-type

  ; Random Number Seeds:
  previous-seed-setup
  previous-seed-go
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

breed [people person]
breed [an-nodes an-node]
directed-link-breed [an-links an-link]
directed-link-breed [an-ties an-tie] ; Ties end1's input node (end2) to end1
directed-link-breed [c-links c-link] ; Linking people to their c-models

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

people-own [
  pp-inputs
  pp-c-models
  pp-prediction
  pp-decision
  pp-action
  pp-target
  pp-freqs
  pp-most-freq-interaction ; ID for recent most frequent type of interaction
  pp-recent-cost
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

an-nodes-own [
  ann-type-id ; See the procedure with the list
  ann-owner
  ann-activation
  ann-sum-weighted-inputs-z
  ann-exp-z ; For softmax
  ann-sorted-in-an-links
  ann-sorted-out-an-links
  ann-delta-activation
  ann-delta-z
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

an-links-own [
  anl-adapts?
  anl-weight
  anl-delta-weight
  anl-delta-input
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic code for an artificial neural networks (ANN)
; including an-nodes, an-links,
; activation, and backpropagation.

to-report new-an-node [given-type-id]
  ; Run by person
  let rep-obj nobody
  hatch-an-nodes 1 [
    set rep-obj self
    set hidden? hide-ann?
    set ann-owner myself
    set ann-type-id given-type-id
    set shape "circle"
    set size 0.5
    set label-color ifelse-value (5 > patch-color mod 10) [white] [black]
    set ann-sorted-in-an-links []
    set ann-sorted-out-an-links []
    set ann-exp-z 0 ; Node plays no role in softmax, unless this is recalculated.
    set ann-activation 1 ; If not a bias node, will be updated before used.
    ;show (word ann-type " : " ([color] of myself) " : " myself)
    set color [5 + 10 * (int (color / 10))] of myself ;scale-color color ann-activation -0.2 1.2
  ]
  report rep-obj
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report new-an-link-from [from-node initial-weight weight-adapts?]
  let rep-obj nobody
  create-an-link-from from-node [
    set rep-obj self
    set hidden? hide-ann?
    set anl-weight initial-weight
    set anl-adapts? weight-adapts?
    set anl-delta-weight 0
    ask end1 [set ann-sorted-out-an-links fput myself ann-sorted-out-an-links]

    ask end2 [create-an-tie-to [end1] of myself [ set hidden? true ]]
  ]
  report rep-obj
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report anl-tie
  ; run as an-link
  ; For every an-link there is a an-tie going in the opposite direction
  report [out-an-tie-to [end1] of myself] of end2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report new-weighted-in-an-links-from [list-of-an-nodes list-of-weights weights-adapts?]
  ; run as end2 an-node
  report (
    map [
      [ann w] ->
      new-an-link-from ann w weights-adapts?
    ]
    list-of-an-nodes
    list-of-weights
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to add-an-links [list-of-from-nodes list-of-to-nodes initial-weight]
  foreach list-of-from-nodes [ego ->
    foreach list-of-to-nodes [alter ->
      ask ego [
        create-an-link-to alter [
          set anl-weight initial-weight
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report sigmoid [z]
  ; The Logistic function
  report 1.0 / (1.0 + exp (- z))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report log-odds [p]
  ; Inverse of sigmoid (the logistic function)
  report ln (p / (1.0 - p))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report weighted-sum [list-of-weights list-of-inputs]
  report sum (map [[w x] -> w * x] list-of-weights list-of-inputs)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-random-bernoulli
  report ifelse-value (ann-activation > random-float 1.0) [1] [0]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to activate-net [given-net given-input-values]
  ; Layer 0 are input nodes
  (foreach (first given-net) given-input-values [
    [ann inp-val] ->
    ask ann [set ann-activation inp-val]
  ])

  ; All other layers come with their own activation functions
  foreach but-first given-net [
    lay ->
    (run (first item 1 lay) first lay) ; inputs-fn: e.g. set z weighted sum of input node activations
    (run (last item 1 lay) first lay) ; transformation-fn: e.g. set activation sigmoid z
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to backpropagate-net [given-reverse-net given-target-values]
  ; Layer 0 are compared with target values
  (foreach (first given-reverse-net) given-target-values [
    [ann targ-val] ->
    set ann-delta-activation
    ann-d-cross-entropy targ-val
  ])

  ; All other layers come with their own activation functions
  foreach but-first given-reverse-net [
    lay ->
    (run (first item 2 lay) first lay) ; z-fn
    (run (item 1 item 2 lay) (first lay) (item 2 item 2 lay) (item 3 item 2 lay)) ; weights-fn, list-of-nodes, learning-rate, weight penalty
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to activate-node
  ; run as an-node
  if empty? ann-sorted-in-an-links [stop] ; No inputs. Leave ann-activation as it was.
  set ann-sum-weighted-inputs-z sum ann-weighted-inputs
  set ann-activation sigmoid ann-sum-weighted-inputs-z
  set color scale-color color ann-activation 1.2 -0.2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to activate-layer-sigmoid [list-of-anns]
  foreach list-of-anns [
    ann ->
    ask ann [activate-node]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to activate-layer-softmax [list-of-anns]
  if empty? list-of-anns [stop]
  foreach list-of-anns [
    ann ->
    ask ann [
      if not empty? ann-sorted-in-an-links [
        set ann-sum-weighted-inputs-z sum map [anl -> [anl-weight * [ann-activation] of end1] of anl] ann-sorted-in-an-links
      ] ; Otherwise, leave z as it is.
    ]
  ]

  let softmax-vals softmax map [ann -> [exp ann-sum-weighted-inputs-z] of ann] list-of-anns
  let denom 1.0 / sum softmax-vals

  (foreach list-of-anns softmax-vals [
    [ann val] ->
    ask ann [
      set ann-activation val * denom
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-softmax-deltas-and-weights [list-of-anns]
  ; For softmax, must update whole layer of nodes
  let delta-ys map [ann ->
    [sum map [anl -> [anl-delta-input] of anl] ann-sorted-out-an-links] of ann ; delta-y = delta E / delta y_j
  ] list-of-anns
  let delta-zs (map [[ann delta-y] ->
    [ann-activation * (1.0 - ann-activation) * delta-y] of ann ; delta-z = delta E / delta z_j
  ] list-of-anns delta-ys)

  (foreach list-of-anns delta-zs [
    [ann delta-z] ->
    ask ann [
      foreach ann-sorted-in-an-links [anl ->
        ask anl [
          set anl-delta-weight delta-z * ([ann-activation] of end1) ; delta E / delta w_ij
          set anl-delta-input anl-weight * delta-z ; Contribution to delta E / delta y_i
        ]
      ]
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-weighted-inputs
  report map [anl -> [anl-weight * [ann-activation] of end1] of anl] ann-sorted-in-an-links
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report softmax [list-of-values]
  let exp-list map [x -> exp x] list-of-values
  if empty? exp-list [report []]
  let mult sum exp-list
  if mult = 0 [
    report n-values (length list-of-values) [-> 1 / length list-of-values] ; Equal probability
  ]
  set mult 1.0 / mult
  report map [x -> x * mult] exp-list

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-cost [given-target]
  report (ann-cross-entropy given-target) + 0.01 * lambda * ann-total-squared-input-weights
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-cross-entropy [given-target]
  ; H(p, q) = -Ep(log q), where Ep(x) is the expected value of x with respect to probability distribution p.
  ; Here: p is target distribution; q is activation

;  ; In nats
;  report (- (
;    given-target * (ln ann-activation) +
;    (1.0 - given-target) * (ln (1.0 - ann-activation))
;  ))

  ; In bits
  report (- (
    given-target * (log ann-activation 2) +
    (1.0 - given-target) * (log (1.0 - ann-activation) 2)
  ))

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-total-squared-input-weights
  report 0.5 * sum map [anl -> ([anl-weight] of anl) ^ 2] ann-sorted-in-an-links
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-d-cross-entropy [given-target]
  ; dC/dy, where C is cross-entropy cost function, and y is activation

  ; nats
  report ((
    (- given-target / ann-activation) +
    ((1.0 - given-target) / (1.0 - ann-activation))
  ))

;  ; bits
;  report ((
;    (- given-target / ann-activation) +
;    ((1.0 - given-target) / (1.0 - ann-activation))
;  )) / ln-2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ln-2
  report ln 2
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to backpropagate-layer-sigmoid [ list-of-nodes ]
  foreach list-of-nodes [
    ann ->
    ask ann [
      set ann-delta-z ann-new-delta-z-sigmoid ann-new-delta-activation
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to backpropagate-layer-softmax [ list-of-nodes target-values]
  ; To be used if softmax is output layer
  ; (otherwise, use backpropagate-layer-sigmoid)

  (foreach list-of-nodes target-values [
    [ann targ-val] ->
    ask ann [
      set ann-delta-z ann-activation - targ-val
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to backpropagate-layer-square [ list-of-nodes ]
  ; y= z ^ 2
  ; dy/dz = 2 * z = 2 * sqrt y
  ; dC/dz = dC/dy * dy/dz

  (foreach list-of-nodes [
    [ann] ->
    ask ann [
      set ann-delta-z 2 * ann-sum-weighted-inputs-z * ann-new-delta-activation ; Quicker?
      ;set ann-delta-z 2 * (sqrt ann-activation) * ann-new-delta-activation
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-layer-in-weights [list-of-nodes given-learning-rate given-weight-penalty]
  foreach list-of-nodes [
    ann-j ->

    foreach [ann-sorted-in-an-links] of ann-j [
      anl-ij ->

      ask [end1] of anl-ij [
        if anl-adapts? [ ; If not frozen
          set anl-weight anl-new-weight given-learning-rate given-weight-penalty
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to backpropagate [given-learning-rate given-weight-penalty]
  ; Run by an-node
  ; delta-y = delta Cost / delta y_j
;  set ann-delta-activation given-error
  set ann-delta-z ann-new-delta-z-sigmoid ann-delta-activation
  foreach ann-sorted-in-an-links [
    anl ->
    ask [end1] of anl [
      set ann-delta-activation ann-new-delta-activation
    ]
    ask anl [
      if anl-adapts? [ ; If not frozen
        set anl-weight anl-new-weight given-learning-rate given-weight-penalty
      ]
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-new-delta-activation
  ; Node takes its weighted share of error from each of its output nodes
  report sum map [
    anl ->
    [anl-weight * [ann-delta-z] of end2] of anl
  ] ann-sorted-out-an-links
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-new-delta-z-sigmoid [given-delta-activation]
  ; Assuming sigmoid node
  report ann-activation * (1.0 - ann-activation) * given-delta-activation
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report anl-new-delta-weight
  ; Activation runs from end1 to end2
  ; so backpropagation flows end2 to end1
  report (
    [ann-activation] of end1 *
    [ann-delta-z] of end2
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report anl-weight-change [given-weight-penalty]
  if (
    self = first [ann-sorted-in-an-links] of end2 ; Bias node
  ) [report anl-new-delta-weight]
  report anl-new-delta-weight + 0.01 * given-weight-penalty * anl-weight
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report anl-new-weight [given-learning-rate given-weight-penalty]
  report anl-weight - 0.01 * given-learning-rate * (anl-weight-change given-weight-penalty)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  let alt-pcolor 1 * ifelse-value (5 > patch-color mod 10) [1] [-1]
  foreach sort patches [pa -> ask pa [set pcolor patch-color + ifelse-value ((pxcor mod 2) = (pycor mod 2)) [alt-pcolor] [0]]]

  setup-rng "seed-setup"
  run Scenario

  set recent-cost 0
  reset-ticks
  set selected-an-node [pp-prediction] of first sorted-people
  ask selected-person [
    foreach [ann-sorted-in-an-links] of pp-prediction [anl ->
      ask [end1] of anl [recolor-c-model]
    ]
  ]

  setup-an-plots
  calc-sorted-people-by-mfi-type
  setup-mfi-plots

  setup-rng "seed-go"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random number generation

to setup-rng [given-variable-name]
  ifelse 0 = runresult given-variable-name [
    run (word "set previous-" given-variable-name " " new-seed)
  ]
  [
    run (word "set previous-" given-variable-name " " given-variable-name)
  ]
  random-seed runresult (word "previous-" given-variable-name)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-people
  set sorted-people sort n-values number-of-people [-> new-person (base-list-of-weights number-of-c-beliefs) true ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report new-person [weights-list-of-lists adapts-weights?]
  let rep-obj nobody
  let num-c-beliefs length item 2 weights-list-of-lists

  create-people 1 [
    set rep-obj self
    set shape "person"
    set label-color ifelse-value (5 > patch-color mod 10) [white] [black]
    setxy random-pxcor random-pycor
    set pp-freqs n-values 4 [-> 0.25]

    ; Define artificial neural network

    set pp-inputs n-values number-of-attributes [-> new-an-node 0]
    set pp-c-models n-values num-c-beliefs [-> new-an-node 4]
    set pp-prediction new-an-node 6
    set pp-decision new-an-node 8
    set pp-recent-cost 1

    let tmp-inputs []
    let tmp-weights []

    ; Prediction to Decision
    set tmp-inputs fput (new-an-node [ann-type-id - 1] of pp-decision) (list pp-prediction)
    set tmp-weights item 0 weights-list-of-lists
    ask pp-decision [
      set ann-sorted-in-an-links new-weighted-in-an-links-from tmp-inputs tmp-weights adapts-weights?
    ]

    ; C-Models to Prediction (Beliefs about Opponent's Hawk-play)
    set tmp-inputs fput (new-an-node [ann-type-id - 1] of pp-prediction) pp-c-models
    set tmp-weights item 1 weights-list-of-lists
    ask pp-prediction [
      set ann-sorted-in-an-links new-weighted-in-an-links-from tmp-inputs tmp-weights adapts-weights?
    ]

    ; Link C-Model to its owner
    foreach pp-c-models [cm ->
      ask cm [
        create-c-link-from myself [
          set hidden? true
          set color [color] of end1
        ]
      ]
    ]

    ; Features to C-Models (Relative utility of attributes to C-Model)
    (foreach pp-c-models (item 2 weights-list-of-lists) [[cm wl] ->
      set tmp-inputs fput (new-an-node [ann-type-id - 1] of cm) n-values number-of-features [->
        (new-an-node [ann-type-id - 2] of cm)
      ]
      set tmp-weights wl
      ask cm [
        set ann-sorted-in-an-links new-weighted-in-an-links-from tmp-inputs tmp-weights adapts-weights?
      ]
    ])

    ; Inputs to C-Model's Features (C-Model's position in attribute-space)
    let cur-person self
    (foreach pp-c-models (item 3 weights-list-of-lists) [[cm wlol] ->
      let rnd-attribs random-coordinates
      (foreach ([but-first ann-sorted-in-an-links] of cm) ([n-values (-1 + length ann-sorted-in-an-links) [k -> k]] of cm) wlol [
        [anl k wl] ->
        set tmp-inputs fput (new-an-node [ann-type-id - 1] of [end1] of anl) (list (item (int (k / 2)) pp-inputs))
        set tmp-weights wl
        ask [end1] of anl [; Feature
          set ann-sorted-in-an-links new-weighted-in-an-links-from tmp-inputs tmp-weights adapts-weights?
        ]
      ])
    ])

    reposition-ann
  ]

  report rep-obj
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report random-coordinates
  report (list
    random-xcor
    random-ycor
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-weights-list-of-lists
  report (list
    [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of pp-decision
    [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of pp-prediction
    map [cm -> [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of cm] pp-c-models
    map [cm -> [map [fea -> [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of [end1] of fea] but-first ann-sorted-in-an-links] of cm] pp-c-models
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to print-ann-weights
  ; run by person
  print ""
  show "In-Weights by layer (Decision Prediction C-Models Features):"
  print "[Bias Weight(s)]"
  (foreach pp-weights-list-of-lists [0 1 2 3] [
    [wl l] ->
    print (word l " : " (map [w -> reprecision-list w 3] wl) " ")
    ]
  )

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report reprecision-list [given-value given-precision]
  if is-list? given-value [report map [v -> reprecision-list v given-precision] given-value ]
  report (precision given-value given-precision)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report an-node-types
  report (list
    "input"
    "bias-to-feature"
    "feature"
    "bias-to-c-model"
    "c-model"
    "bias-to-prediction"
    "prediction"
    "bias-to-action"
    "action"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-type
  report item ann-type-id an-node-types
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-c-model
  ; run as c-model
  ;let cmod-pos position (out-an-link-to [pp-prediction] of ann-owner) [ann-sorted-in-an-links] of [pp-prediction] of ann-owner
  let cmod self
  let cmod-pos [position (in-an-link-from cmod) ann-sorted-in-an-links] of [pp-prediction] of ann-owner

  let cmod-color item cmod-pos c-model-colors
  set color cmod-color
  foreach ann-sorted-in-an-links [
    feal ->
    ask [end1] of feal [
      set color cmod-color
      if not empty? ann-sorted-in-an-links [ ; Not a bias node
        ask [end1] of first ann-sorted-in-an-links [
          set color cmod-color
        ]
      ]
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-ann
  ; run as person
  let my ifelse-value (ycor > mean list max-pycor min-pycor) [-1] [1] ; Put an-nodes below or above person?

  let mid-y my * (1 + length pp-c-models)

  ask pp-decision [
    set shape "hex"
    move-to myself
    setxy (xcor + 2) (ycor + mid-y)

    ; Bias and Prediction as inputs to Decision
    let anchor self
    let num-in length ann-sorted-in-an-links
    (foreach ann-sorted-in-an-links (n-values num-in [k -> k]) [
      [anl k] ->
      ask [end1] of anl [
        if empty? ann-sorted-in-an-links [set shape "triangle"] ; bias node
        move-to anchor
        setxy (xcor - 1) (ycor - my * k + my * 0.5 * num-in)
      ]
    ])
  ]

  ask pp-prediction [
    ;move-to myself
    ;setxy (xcor + 1) (ycor + mid-y)

    ; Bias and C-Models as inputs to Prediction
    let anchor self
    let num-in length ann-sorted-in-an-links
    (foreach ann-sorted-in-an-links (n-values num-in [k -> k]) [
      [anl k] ->
      ask [end1] of anl [ ; Bias or C-Model
        if empty? ann-sorted-in-an-links [set shape "triangle"] ; bias node
        move-to anchor
        setxy (xcor - 1) (ycor - my * 0.5 * (1 + number-of-features) * k + my * 0.25 * (1 + number-of-features) * num-in)
      ]
    ])
  ]

  ; Input nodes
  let num-i (length pp-inputs)
  (foreach pp-inputs (n-values num-i [k -> k]) [
    [ann k] ->
    ask ann [
      set shape "square"
      move-to myself
      setxy (xcor - 3) (ycor + mid-y + my * 0.5 * (-1 + num-i) - my * k)
    ]
  ])

  foreach pp-c-models [
    cm ->

    ; Bias and Features as inputs to C-Models
    let num-in length [ann-sorted-in-an-links] of cm
    (foreach ([ann-sorted-in-an-links] of cm) (n-values num-in [k -> k]) [
      [anl k] ->
      ask [end1] of anl [ ; Feature or Bias
        if empty? ann-sorted-in-an-links [set shape "triangle"] ; bias node
        move-to cm
        setxy (xcor - 1) (ycor - 0.5 * my * k + my * 0.25 * num-in)

        ; Bias to compare with Weighted Input
        if not empty? ann-sorted-in-an-links [ ; If a feature, not a bias
          let anchor self
          ask [end1] of first ann-sorted-in-an-links [ ; Bias, not an Input node
            set shape "triangle"
            move-to anchor
            setxy (xcor - 1) (ycor)
          ]
          ask [anl-tie] of first ann-sorted-in-an-links [tie] ; Tie bias to feature, so it moves if feature (and c-model) moves
        ]
      ]
      ask [anl-tie] of anl [tie] ; Tie features and biases to c-models so that they move if c-model moves
    ])

  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-all-c-models
  ; run as person

  foreach pp-c-models [
    cm ->
    ask cm [reposition-c-model]
  ]

  ; Move all other nodes to be nearer person
  ask pp-prediction [
    move-to myself setxy (xcor + 1) ycor
    ask [end1] of first ann-sorted-in-an-links [ ; Bias node to predicition
      move-to myself setxy (xcor - 1) (ycor + 1)
    ]
  ]

  ask pp-decision [
    move-to myself setxy (xcor + 2) ycor
    ask [end1] of first ann-sorted-in-an-links [ ; Bias node to decision
      move-to myself setxy (xcor - 1) (ycor + 1)
    ]
  ]

  let num-i (length pp-inputs)
  (foreach pp-inputs (n-values num-i [k -> k]) [
    [ann k] ->
    ask ann [
      set shape "square"
      move-to myself
      setxy (xcor - 1) (ycor + 0.5 * (-1 + num-i) - k)
    ]
  ])

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-c-model
  ; run as c-model, assumes first 2 features relate to xcor, second 2 features relate to ycor
  setxy (
    mean map [anl -> [(- [anl-weight] of first ann-sorted-in-an-links) / ([anl-weight] of item 1 ann-sorted-in-an-links)] of [end1] of anl] sublist ann-sorted-in-an-links 1 3
    ) (
    mean map [anl -> [(- [anl-weight] of first ann-sorted-in-an-links) / ([anl-weight] of item 1 ann-sorted-in-an-links)] of [end1] of anl] sublist ann-sorted-in-an-links 3 5
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to activate-pp-inputs [given-opponent]
  (foreach pp-inputs ([pp-attributes] of given-opponent) [
    [ann att] ->
    ask ann [
      set ann-activation att
    ]
    ]
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to activate-c-models
  foreach pp-c-models [cm ->
    ask cm [
      foreach ann-sorted-in-an-links [anl ->
        ask [end1] of anl [activate-node] ; Asking feature
      ]
      activate-node
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report initial-belief-total-weighted-activation
  ; z = log odds = ln (p / (1-p))
  ; z = b0 + b1*x1; b0 = 0; x1 = 1. b1 = z
  report ln (msne / (100 - msne))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report number-of-attributes
  report 2 ; length pp-attributes
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report number-of-features
  ; Features are paired for each attribute:
  ; One lower bound, one upper bound.
  report 2 * number-of-attributes
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-attributes
  report (list xcor ycor)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to simulate-interaction [ego alter]
  ask ego [choose-action-against alter]
  ask alter [choose-action-against ego]

  ask ego [update-frequency interaction-type-id pp-action ([pp-action] of alter)]
  ask alter [update-frequency interaction-type-id pp-action ([pp-action] of ego)]

  ask ego [learn-from alter]
  ask alter [learn-from ego]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to choose-action-against [alter]
  activate-pp-inputs alter
  activate-c-models
  ask pp-prediction [activate-node]

;  ; Using decision.
;  ; Treat its activation as a probability for a Bernoulli-distributed mixed strategy.
;  ask pp-decision [activate]
;  set pp-action [ann-random-bernoulli] of pp-decision

  ; Using prediction, skipping decision node and using a fixed rule.
  set pp-action (ifelse-value
    (msne > 100 * [ann-activation] of pp-prediction) [1]
    (msne < 100 * [ann-activation] of pp-prediction) [0]
    (ifelse-value (msne > random-float 100) [1] [0])
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to learn-from [given-opponent]
  ; run as person
  set pp-target [pp-action] of given-opponent
  update-cost

  ; Using decision node.
;  ask pp-decision [
;    set ann-delta-activation
;    ann-d-cross-entropy (1 - [pp-target] of myself) ; Mis-matching game
;    backpropagate 0 ; Assume payoff function is not changing, so no re-learning required.
;  ]
;  ask pp-prediction [backpropagate (100 - memory)]

  ; Skipping decision, using prediction only.
  ask pp-prediction [
    set ann-delta-activation
    ann-d-cross-entropy [pp-target] of myself ; Belief is about opponent's action

    backpropagate (100 - memory) lambda
  ]

  ; Relevance learning
  foreach pp-c-models [
    cm ->
    ask cm [
      backpropagate (100 - Model-Retentiom) lambda
      foreach ann-sorted-in-an-links [
        anl ->
        ask [end1] of anl [ ; Feature node
          backpropagate (100 - inertia) lambda
        ]
      ]
    ]
  ]

  if Reposition-C-Models? [
    if not empty? pp-c-models [
      if [not hidden?] of first pp-c-models [
        reposition-all-c-models
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-frequency [given-outcome-type-id]
  ; Run as person
  set pp-freqs map [
    f ->
    0.01 * f * statistics-retention
  ] pp-freqs
  set pp-freqs replace-item given-outcome-type-id pp-freqs (
    0.01 * (100 - statistics-retention) + item given-outcome-type-id pp-freqs
  )
  set pp-most-freq-interaction pp-most-freq-interaction-calculated
  person-recolor-by-most-freq-interaction
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report interaction-type-id [my-action your-action]
  report your-action + 2 * my-action
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-cost
  set pp-recent-cost 0.01 * (
    statistics-retention * pp-recent-cost +
    (100 - statistics-retention) * [ann-cost [pp-target] of ann-owner] of pp-prediction
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; reposition an-nodes, inputs + bias, features + bias, c-models + bias, prediction + bias, action
; ?learn decision rule given mis-match score
; Either: learn prediction given action of opponent
; Or: learn prediction given action error
; Learning relevance given prediction error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  let pairing shuffle sorted-people
  (foreach
    (sublist pairing 0 (0.5 * length pairing))
    (sublist pairing (0.5 * length pairing) length pairing)
    [
      [ego alter] ->
      simulate-interaction ego alter
    ]
  )

  tick
  do-an-plots
  calc-sorted-people-by-mfi-type
  do-mfi-plots
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report mfi-type-labels
  report (list "DD" "DH" "HD" "HH")
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report interaction-colors
  ; Both D, DH, HD, HH
  report (list dd-color dh-color hd-color hh-color)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report interaction-shapes
  ; Both D, DH, HD, HH
  report (list "circle" "triangle" "square" "x")
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to person-recolor-by-most-freq-interaction
  set color item pp-most-freq-interaction interaction-colors
  set shape item pp-most-freq-interaction interaction-shapes
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-most-freq-interaction-calculated
  let max-val max pp-freqs
  report one-of filter [a -> max-val = item a pp-freqs] [0 1 2 3]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-mfi-plots
  set-current-plot "Most Frequent Interaction"
  (foreach mfi-pen-names interaction-colors [
    [pen-nam pen-col] ->
    create-temporary-plot-pen pen-nam
    set-plot-pen-color pen-col
  ])

  set-current-plot "Mean Cost by MFI"
  (foreach mfi-pen-names interaction-colors [
    [pen-nam pen-col] ->
    create-temporary-plot-pen pen-nam
    set-plot-pen-color pen-col
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report mfi-pen-names
  report (list
    "DD: Peace"
    "DH: Dominated"
    "HD: Dominator"
    "HH: Conflict"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to do-mfi-plots
  set-current-plot "Most Frequent Interaction"
  let cur-percs map [m -> perc-interaction-type m] [0 1 2 3]

  (foreach [0 1 2 3] cur-percs [
    [m p] ->
    set-current-plot-pen item m mfi-pen-names
    plotxy ticks p
  ])

  set-current-plot "Mean Cost by MFI"
  let cur-total 0
  let cur-cost 0
  (foreach [0 1 2 3] cur-percs [
    [m p] ->
    set-current-plot-pen item m mfi-pen-names
    set cur-cost mean-cost-by-mfi m
    if 300 < round (p * count people) [ ; Don't plot unless statistic based on at least 4 people
      plotxy ticks cur-cost
    ]
    set cur-total cur-total + cur-cost * length item m sorted-people-by-mfi-type
  ])
  set-current-plot-pen "All Pop"
  plotxy ticks (cur-total / count people)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-interaction-type [given-type]
  report 100 * (length item given-type sorted-people-by-mfi-type) / length sorted-people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report mean-cost-by-mfi [given-type]
  if empty? item given-type sorted-people-by-mfi-type [report 0]
  report mean map [pp -> [pp-recent-cost] of pp] item given-type sorted-people-by-mfi-type
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calc-sorted-people-by-mfi-type
  set sorted-people-by-mfi-type map [mfi ->
    sorted-people-of-mfi-type-calculated mfi
  ] n-values 4 [mfi -> mfi]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report sorted-people-of-mfi-type-calculated [given-type]
  report filter [pp -> [given-type = pp-most-freq-interaction] of pp] sorted-people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report selected-person
  if not is-an-node? selected-an-node [report nobody]
  report [ann-owner] of selected-an-node
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to select-person
  if mouse-inside? [
    if mouse-down? [
      set selected-an-node nobody
      if any? (people with [not hidden?]) [
        let nearest-turtle min-one-of (people with [not hidden?]) [distancexy mouse-xcor mouse-ycor]
        if [distancexy mouse-xcor mouse-ycor] of nearest-turtle < 0.5[
          set selected-an-node [pp-prediction] of nearest-turtle
        ]
      ]
      if is-an-node? selected-an-node [
        setup-an-plots
        stop
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to select-an-node
  if mouse-inside? [
    if mouse-down? [
      set selected-an-node nobody
      if any? (an-nodes with [not hidden?]) [
        let nearest-turtle min-one-of (an-nodes with [not hidden?]) [distancexy mouse-xcor mouse-ycor]
        if [distancexy mouse-xcor mouse-ycor] of nearest-turtle < 0.5[
          set selected-an-node nearest-turtle
        ]
      ]
      if is-an-node? selected-an-node [
        setup-an-plots
        stop
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-an-plots
  set-current-plot "Input Weights of AN-Node"
  clear-plot
  set-current-plot "Activations of AN-Node"
  clear-plot
  set-current-plot "Cost"
  clear-plot
  if not is-an-node? selected-an-node [stop]

  ask selected-an-node [
    set-current-plot "Input Weights of AN-Node"
    (foreach (n-values (length ann-sorted-in-an-links) [k -> k]) [
      [k] ->
      create-temporary-plot-pen (word k)
      set-plot-pen-color item k c-model-colors
    ])

    set-current-plot "Activations of AN-Node"
    (foreach (n-values (length ann-sorted-in-an-links) [k -> k]) [
      [k] ->
      create-temporary-plot-pen (word k)
      set-plot-pen-color item k c-model-colors
    ])

    set-current-plot "Logistic Fn by AN-Node Input"
    (foreach (n-values (length ann-sorted-in-an-links) [k -> k]) [
      [k] ->
      create-temporary-plot-pen (word k)
      set-plot-pen-color item k c-model-colors
    ])
  ]

  do-an-plots

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report ann-x-range
  (ifelse
    (ann-type-id = 8) [report (range 0 1.01 0.01)] ; Decision. Input is from Prediction.
    (ann-type-id = 6) [report (range 0 1.01 0.01)] ; Prediction. Input is from C-Model(s).
    (ann-type-id = 4) [report (range 0 1.01 0.01)] ; C-Model. Input is from Features.
    (ann-type-id = 2) [report (range (min-pxcor - 0.5) (0.1 + max-pxcor + 0.5) 0.1)] ; Feature. Input is from attribute. NB: Assumes ycor range same as xcor range.
  )
  report (range -6 6.1 0.1)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-hidden-for-ann [given-setting]
  ask pp-decision [
    set-hidden-for-ann-inputs given-setting
  ]
  foreach pp-inputs [
    ann ->
    ask ann [
      if empty? ann-sorted-out-an-links [ ; No out links, so not called via pp-decision
        set-hidden-for-ann-inputs given-setting
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-hidden-for-ann-inputs [given-setting]
  ; run by an-node
  set hidden? given-setting
  foreach ann-sorted-in-an-links [
    anl ->
    ask anl [set hidden? given-setting]
    ask [end1] of anl [set-hidden-for-ann-inputs given-setting] ; Recursive
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to toggle-hidden-ann
  if mouse-inside? [
    if mouse-down? [
      let mouse-people [people-here] of patch (round mouse-xcor) (round mouse-ycor)
      if any? mouse-people [
        foreach sort mouse-people [
          pp ->
          ask pp [
            set-hidden-for-ann [not hidden?] of pp-decision
          ]
          set selected-an-node [pp-prediction] of pp
        ]
        stop
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report c-model-colors
  report (list
    red (yellow - 1) lime (blue - 2) violet pink orange (green - 1) sky cyan magenta turquoise brown
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to do-an-plots
  if not is-an-node? selected-an-node [stop]

  ask selected-an-node [
    set-current-plot "Input Weights of AN-Node"
    set-current-plot-pen "y = 0"
    ifelse 0 = (int (ticks / 10)) mod 2 [plot-pen-down] [plot-pen-up]
    plotxy ticks 0
    (foreach ann-sorted-in-an-links (n-values (length ann-sorted-in-an-links) [k -> k]) [
      [anl k] ->
      set-current-plot-pen (word k)
      plotxy ticks [anl-weight] of anl
    ])

    set-current-plot "Activations of AN-Node"
    (foreach ann-sorted-in-an-links (n-values (length ann-sorted-in-an-links) [k -> k]) [
      [anl k] ->
      set-current-plot-pen (word k)
      plotxy ticks [ann-activation] of [end1] of anl
    ])
    set-current-plot-pen "Output"
    plotxy ticks ann-activation

    set-current-plot "Cost"
    if ann-activation > 0 [
      if ann-activation < 1 [
        set recent-cost 0.01 * (
          statistics-retention * recent-cost +
          (100 - statistics-retention) * ann-cost [pp-target] of ann-owner
        )
        plotxy ticks recent-cost
      ]
    ]

    set-current-plot "Logistic Fn by AN-Node Input"
    if Clear-Plot-Each-Node? [clear-plot]
    if not empty? ann-sorted-in-an-links [
      let x-vals ann-x-range
      set-plot-x-range (min x-vals) (max x-vals)
      foreach n-values (length ann-sorted-in-an-links) [k -> k] [
        k ->
        ;set-current-plot-pen (word k)
        create-temporary-plot-pen (word k)
        set-plot-pen-color item k c-model-colors
        foreach x-vals [
          x ->
          plotxy x (
            sigmoid weighted-sum (
              map [anl -> [anl-weight] of anl] ann-sorted-in-an-links
              ) (
              fput 1 map [i -> ifelse-value (k = i) [x] [0]] n-values (-1 + length ann-sorted-in-an-links) [i -> i]
            )
          )
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to test1
  let adaptive-group n-values 1 [-> new-person (base-list-of-weights number-of-c-beliefs) true]
  let group-a n-values (0.25 * (number-of-people - 1)) [-> new-person (base-list-of-weights 0) false]
  let group-b n-values (number-of-people - 1 - length group-a) [-> new-person (base-list-of-weights 0) false]

  foreach adaptive-group [
    pp ->
    ask pp [
      set-hidden-for-ann false
      set color red
      setxy 0 0

      reposition-ann
      if Reposition-C-Models? [reposition-all-c-models]
    ]
  ]

  foreach group-a [
    pp ->
    ask pp [
      set color blue - 1
      setxy (random-float max-pxcor) (random-float max-pycor)
      reposition-ann
      ask [first ann-sorted-in-an-links] of pp-prediction [
        set anl-weight log-odds 0.001 ; Low expectations of Hawk. Will therefore play Hawk
      ]
    ]
  ]

  foreach group-b [
    pp ->
    ask pp [
      set color lime
      setxy (- random-float abs min-pxcor) (- random-float abs min-pycor)
      reposition-ann
      ask [first ann-sorted-in-an-links] of pp-prediction [
        set anl-weight log-odds 0.999 ; High expectations of Hawk. Will therefore play Dove
      ]
    ]
  ]

  set sorted-people (sentence
    adaptive-group
    group-a
    group-b
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to test2
  let adaptive-group n-values 1 [-> new-person (base-list-of-weights number-of-c-beliefs) true]
  let group-a n-values (0.25 * (number-of-people - 1)) [-> new-person (base-list-of-weights 0) false]
  let group-b n-values (0.25 * (number-of-people - 1)) [-> new-person (base-list-of-weights 0) false]
  let group-c n-values (0.25 * (number-of-people - 1)) [-> new-person (base-list-of-weights 0) false]
  let group-d n-values (number-of-people - count people) [-> new-person (base-list-of-weights 0) false]

  foreach adaptive-group [
    pp ->
    ask pp [
      set-hidden-for-ann false
      set color red
      setxy 0 0
      reposition-ann
      if Reposition-C-Models? [reposition-all-c-models]
    ]
  ]

  foreach group-a [
    pp ->
    ask pp [
      set color blue - 1
      setxy (- random-float abs min-pxcor) (random-float max-pycor)
      reposition-ann
      ask [first ann-sorted-in-an-links] of pp-prediction [
        set anl-weight log-odds 0.001 ; Low expectations of Hawk. Will therefore play Hawk
      ]
    ]
  ]

  foreach group-b [
    pp ->
    ask pp [
      set color lime
      setxy (random-float max-pxcor) (random-float max-pycor)
      reposition-ann
      ask [first ann-sorted-in-an-links] of pp-prediction [
        set anl-weight log-odds 0.999 ; High expectations of Hawk. Will therefore play Dove
      ]
    ]
  ]

  foreach group-c [
    pp ->
    ask pp [
      set color lime
      setxy (- random-float abs min-pxcor) (- random-float abs min-pycor)
      reposition-ann
      ask [first ann-sorted-in-an-links] of pp-prediction [
        set anl-weight log-odds 0.999 ; High expectations of Hawk. Will therefore play Dove
      ]
    ]
  ]

  foreach group-d [
    pp ->
    ask pp [
      set color blue - 1
      setxy (random-float max-pxcor) (- random-float abs min-pycor)
      reposition-ann
      ask [first ann-sorted-in-an-links] of pp-prediction [
        set anl-weight log-odds 0.001 ; Low expectations of Hawk. Will therefore play Hawk
      ]
    ]
  ]

  set sorted-people (sentence
    adaptive-group
    group-a
    group-b
    group-c
    group-d
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to base-population
  set sorted-people n-values number-of-people [-> new-person (base-list-of-weights number-of-c-beliefs) true]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report base-list-of-weights [num-c-models]
  ; Foreach layer with inputs, working backwards from Decision to Features,
  ; for each node in layer,
  ; provide list weights.
  ; First weight in each list is for bias node.
  ; Decision and Prediction layers currently only have 1 node, so simplified them.

  ; Maths for converting parameterisations:
  ; Let c = centre, s = slope,
  ; and b0 and b1 be parameters to linear equation y= b0 + b1*x
  ; (x - c) * s = s*x - s*c
  ; b0 = -s*c
  ; b1 = s
  ; c = - b0/b1

  report (list
    ; To Decision:
    (list (-0.01 * msne * -50) -50) ; -50 gives steep slope. Aim to deliver activations either close to 0 (pred>msne) or close to 1 (pred<msne)

    ; To Prediction:
    (fput (
      log-odds (0.01 * msne)
    ) n-values num-c-models [i -> random-normal 0 0.01]) ; Default prediction = msne. Even if activated, C-Models are initially neutral.

    ; To C-Models:
    (n-values num-c-models [i ->
      (fput (
        6 * (- number-of-features + 0.5)
      ) n-values number-of-features [-> 6]) ; Slope = 6 (steep). All features must be activated to activate C-Model.
    ])

    ; C-Models' Features:
    (n-values num-c-models [j ->
      (reduce sentence map [a -> ; Attribute value
        (list
          (list (- (a - Feature-Coverage) * Feature-Slope) Feature-Slope) ; Lower bound for attribute value
          (list ((a + Feature-Coverage) * Feature-Slope) (- Feature-Slope)) ; Upper bound for attribute value
        )
        ]
      random-coordinates ; Each C-Model represents a paradigm, ideal type or stereotypical person with this position in attribute-space.
      )
    ])
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
220
10
636
427
-1
-1
24.0
1
10
1
1
1
0
1
1
1
-8
8
-8
8
0
0
1
ticks
30.0

INPUTBOX
7
106
159
166
Number-Of-People
100.0
1
0
Number

BUTTON
5
265
66
299
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
210
178
243
MSNE
MSNE
0
100
90.0
5
1
%
HORIZONTAL

SLIDER
10
495
182
528
Lambda
Lambda
0
100
0.0
5
1
%
HORIZONTAL

SLIDER
10
385
182
418
Memory
Memory
0
100
90.0
5
1
%
HORIZONTAL

SLIDER
10
455
182
488
Inertia
Inertia
0
100
90.0
5
1
%
HORIZONTAL

SLIDER
5
170
177
203
Number-Of-C-Beliefs
Number-Of-C-Beliefs
0
10
3.0
1
1
NIL
HORIZONTAL

TEXTBOX
10
10
220
87
Game Players who use Artificial Neural Networks (ANNs)
20
0.0
1

TEXTBOX
10
85
213
104
(C) Christopher J Watts, 2025.
11
0.0
1

SLIDER
10
530
183
563
Statistics-Retention
Statistics-Retention
0
100
90.0
5
1
%
HORIZONTAL

BUTTON
5
305
69
339
Go 1
Go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
135
265
205
299
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

INPUTBOX
10
970
162
1030
Patch-Color
9.9
1
0
Color

INPUTBOX
10
795
162
855
Seed-Setup
0.0
1
0
Number

INPUTBOX
10
860
162
920
Seed-Go
0.0
1
0
Number

BUTTON
165
815
267
848
Use Previous
set seed-setup previous-seed-setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
270
815
332
848
Clear
set seed-setup 0
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
165
875
267
908
Use Previous
set seed-go previous-seed-go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
270
875
332
908
Clear
set seed-go 0
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
10
765
195
796
Random Number Seeds:
14
0.0
1

BUTTON
400
470
562
503
Label Nodes by Type-ID
foreach sort an-nodes [\nann -> \nask ann [ set label (word ann-type-id \" \")]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
400
505
527
538
Clear Node Labels
foreach sort an-nodes [\nann -> \nask ann [ set label \"\"]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
220
505
362
538
Reposition C-Models
foreach sorted-people [\npp ->\nask pp [\nreposition-all-c-models\n]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
220
470
337
503
Reposition ANN
foreach sorted-people [\npp ->\nask pp [\nreposition-ann\n]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SWITCH
180
970
292
1003
Hide-ANN?
Hide-ANN?
0
1
-1000

PLOT
775
10
1025
175
Input Weights of AN-Node
Ticks
ANL-Weight
0.0
1.0
-1.0
1.0
true
true
"" ""
PENS
"y = 0" 1.0 0 -7500403 true "" ""

BUTTON
70
305
132
338
Go 10
repeat 10 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
135
305
202
338
Go 100
repeat 100 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
1030
10
1310
175
Activations of AN-Node
Ticks
Activation
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"Output" 1.0 0 -16777216 true "" ""

MONITOR
645
55
760
100
NIL
Selected-AN-Node
17
1
11

BUTTON
645
15
762
48
NIL
Select-AN-Node
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
645
105
760
150
ANN-Tyoe
[ann-type] of selected-an-node
17
1
11

MONITOR
645
155
760
200
ANN-Owner
[ann-owner] of selected-an-node
17
1
11

BUTTON
645
330
767
363
Hide/Unhide ANN
toggle-hidden-ann
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
1560
10
1840
175
Logistic Fn by AN-Node Input
Input
Activation
-3.0
3.0
0.0
1.0
true
true
"" ""
PENS
"0" 1.0 0 -16777216 true "" ""

MONITOR
775
185
1025
230
Input Weights
[map [\nanl -> (word [precision anl-weight 3] of anl \" \")\n] ann-sorted-in-an-links] of Selected-AN-Node
17
1
11

MONITOR
1030
185
1310
230
Input Activations
[map [\nanl -> (word [precision ann-activation 3] of [end1] of anl \" \")\n] ann-sorted-in-an-links] of Selected-AN-Node
17
1
11

BUTTON
800
235
1017
268
Print Weights of AN-Node's Owner
ask [ann-owner] of selected-an-node [print-ann-weights]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
10
665
135
725
Feature-Coverage
4.0
1
0
Number

SWITCH
1560
180
1737
213
Clear-Plot-Each-Node?
Clear-Plot-Each-Node?
0
1
-1000

INPUTBOX
140
665
250
725
Feature-Slope
1.25
1
0
Number

BUTTON
645
375
747
408
Select Person
select-person
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
10
420
182
453
Model-Retentiom
Model-Retentiom
0
100
90.0
5
1
%
HORIZONTAL

SWITCH
220
545
392
578
Reposition-C-Models?
Reposition-C-Models?
0
1
-1000

CHOOSER
10
595
148
640
Scenario
Scenario
"Base-Population" "Test1" "Test2"
0

PLOT
1315
10
1555
175
Cost
Ticks
Recent Cost
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"Recent" 1.0 0 -16777216 true "" ""

BUTTON
605
470
727
503
Recolor by Belief
foreach sorted-people [\npp ->\nask pp [ set color scale-color red (\n[ann-activation] of pp-prediction\n) -0.2 1.2 ]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
645
235
780
280
Freqs (%)
[map [f -> precision (100 * f) 1] pp-freqs] of selected-person
17
1
11

INPUTBOX
335
970
487
1030
DD-Color
65.0
1
0
Color

INPUTBOX
490
970
642
1030
DH-Color
44.0
1
0
Color

INPUTBOX
335
1035
487
1095
HD-Color
104.0
1
0
Color

INPUTBOX
490
1035
642
1095
HH-Color
15.0
1
0
Color

PLOT
780
295
1095
480
Most Frequent Interaction
Ticks
% of Pop
0.0
1.0
0.0
100.0
true
true
"" ""
PENS

MONITOR
780
485
967
530
People by MFI Type (% of Pop)
map [x -> precision ((100 * length x) / count people) 1] sorted-people-by-mfi-type
17
1
11

PLOT
1100
295
1415
480
Mean Cost by MFI
Ticks
Cost
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"All Pop" 1.0 0 -16777216 true "" ""

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

hex
false
0
Polygon -7500403 true true 0 150 75 30 225 30 300 150 225 270 75 270

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
