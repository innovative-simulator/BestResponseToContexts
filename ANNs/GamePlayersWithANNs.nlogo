;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Players who use Artifical Neural Networks
;; (C) Christopher J Watts, 2025.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [table profiler]
__includes ["ANNet.nls"]

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
directed-link-breed [c-links c-link] ; Linking people to their c-models

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

people-own [
  pp-learns? ; Does person undergo backpropagation?
  pp-net ; List of layers. Layer is a list, first item is a list of an-nodes.
  pp-inputs ; Nodes in layer 0
  pp-features ; Nodes in layer 2
  pp-c-models ; Nodes in layer 4
  pp-predictions ; Nodes in layer 6
  pp-decision ; Nodes in layer 8
  pp-action ; Actual move made in game, following decision.
  pp-target ; Opponent's action to be predicted (if not using Decision nodes), or my ideal action, given opponent's move (using Decision)
  pp-freqs ; Counts, decaying over time, of interaction experiences.
  pp-most-freq-interaction ; ID for recent most frequent type of interaction
  pp-recent-cost ; Cross-entropy loss, decaying over time.
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  let alt-pcolor 1 * ifelse-value (5 > patch-color mod 10) [1] [-1]
  foreach sort patches [pa -> ask pa [set pcolor patch-color + ifelse-value ((pxcor mod 2) = (pycor mod 2)) [alt-pcolor] [0]]]

  setup-rng "seed-setup"
  run Scenario

  setup-annet

  set recent-cost 0
  reset-ticks
  set selected-an-node [first pp-predictions] of first sorted-people
  ask selected-person [
    foreach [ann-sorted-in-an-links] of selected-an-node [anl ->
      if Recolor-AN-Nodes? [ask [end1] of anl [recolor-c-model]]
    ]
  ]

  if Label-AN-Nodes? [relabel-all-an-nodes]

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
    set pp-learns? true
    set pp-freqs n-values 4 [-> 0.25]

    ; Define artificial neural network

    ; Neural Network is a list of layers
    set pp-net base-node-layer-list
    ; list-of-nodes is first item of each layer
    set pp-inputs first item 0 pp-net
    set pp-features first item 2 pp-net
    set pp-c-models first item 4 pp-net
    set pp-predictions first item 6 pp-net
    set pp-decision first item 8 pp-net
    set pp-recent-cost 1

    ; Link C-Model to its owner
    foreach pp-c-models [cm ->
      ask cm [
        create-c-link-from myself [
          set hidden? true
          set color [color] of end1
        ]
      ]
    ]

    ; Make the links from layer to layer
    make-an-links-one-to-many pp-inputs pp-features (number-of-features / length pp-inputs) false
    make-an-links-one-to-one (first item 1 pp-net) pp-features true
    make-an-links-many-to-one pp-features pp-c-models number-of-features false
    make-an-links-one-to-one (first item 3 pp-net) pp-c-models false
    make-an-links-all-to-one pp-c-models pp-predictions true
    make-an-links-one-to-one (first item 5 pp-net) pp-predictions true
    make-an-links-all-to-one pp-predictions pp-decision false
    make-an-links-one-to-one (first item 7 pp-net) pp-decision false  ;false, because not learning payoffs.

    ; Set node shape to reflect layer / role
    foreach (list
      (list 0 "square")
      (list 1 "triangle")
      (list 2 "circle")
      (list 3 "triangle")
      (list 4 "pentagon")
      (list 5 "triangle")
      (list 6 "circle")
      (list 7 "triangle")
      (list 8 "circle")
    ) [
      layer-shape ->
      foreach first item (first layer-shape) pp-net [
        ann ->
        ask ann [set shape last layer-shape]
      ]
    ]

    initialize-weights base-list-of-weights length pp-c-models
  ]
  report rep-obj
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-net [given-net given-spacings]
  ; Run as person
  ; Person has xcor, ycor, pp-net

  ; Where is the most space? Above me or below? To the left or to the right?
  let up-not-down ifelse-value (ycor > mean (list max-pycor min-pycor)) [-1] [1]
  let right-not-left ifelse-value (xcor > mean (list max-pxcor min-pxcor)) [-1] [1]
  let max-lay-size max map [lay -> length first lay] given-net

  let nlayers length pp-net
  (foreach given-net (n-values (length given-net) [n -> n]) given-spacings [
    [lay n spcing] ->

    let x xcor + right-not-left * n ; All layer's nodes in same column
    let lay-size length first lay ; Number of nodes in layer
    reposition-layer lay x (ycor + up-not-down * 0.5 * max-lay-size * spcing + ifelse-value (0 = n mod 2) [-0.25] [0.25]) up-not-down spcing

  ])

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-layer [lay x mid-y up-not-down spcing]
  let lay-size length first lay ; Number of nodes in layer

  (foreach (first lay) (n-values lay-size [k -> k]) [
    [ann k] ->
    ask ann [
      move-to ann-owner ; If not there already
      setxy x (
        mid-y + up-not-down * spcing * (0.5 * lay-size - k )
      )
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report net-layer-spacings
  report n-values (length pp-net) [-> 0.75]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to tie-to-c-models
  ; Run as person
  ; Tie features and bias nodes to c-models,
  ; so that if c-model moves, features go with it.
  foreach pp-c-models [
    ann ->
    foreach ([ann-sorted-in-an-links] of ann) [
      anl ->
      ask [anl-tie] of anl [tie]
    ]
  ]
  foreach pp-features [
    ann ->
    let anl [first ann-sorted-in-an-links] of ann ; Tie bias node only, not input node
    ask [anl-tie] of anl [tie]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to untie-to-c-models
  ; Run as person
  ; Untie features and bias nodes to c-models,
  ; so that if c-model moves, features don't go with it.
  foreach pp-c-models [
    ann ->
    foreach ([ann-sorted-in-an-links] of ann) [
      anl ->
      ask [anl-tie] of anl [untie]
    ]
  ]
  foreach pp-features [
    ann ->
    let anl [first ann-sorted-in-an-links] of ann ; Tie bias node only, not input node
    ask [anl-tie] of anl [untie]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to make-an-links-one-to-many [from-list to-list to-ratio updateable-weights?]
  ; E.g. one input is received by many features
  (foreach to-list (n-values (length to-list) [j -> j]) [
    [ann j] ->
    ask ann [
      set ann-sorted-in-an-links fput (
        new-an-link-from (
          item ((int (j / to-ratio)) mod length from-list) from-list
        ) 0 updateable-weights?
      ) ann-sorted-in-an-links
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to make-an-links-one-to-one [from-list to-list updateable-weights?]
  ; (Could also be done as one-to-many with to-ratio = 1)
  (foreach to-list from-list [
    [ann from-node] ->
    ask ann [
      set ann-sorted-in-an-links fput (
        new-an-link-from (
          from-node
        ) 0 updateable-weights?
      ) ann-sorted-in-an-links
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to make-an-links-many-to-one [from-list to-list from-ratio updateable-weights?]
  ; E.g. many features are combined in one model
  (foreach to-list (n-values (length to-list) [j -> j]) [
    [ann j] ->
    ask ann [
      foreach (n-values from-ratio [k -> k]) [
        k ->
        set ann-sorted-in-an-links fput (
          new-an-link-from (
            item ((j * from-ratio + k) mod length from-list) from-list
          ) 0 updateable-weights?
        ) ann-sorted-in-an-links
      ]
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to make-an-links-all-to-one [from-list to-list updateable-weights?]
  ; E.g. All models advise on a prediction, or on a decision.
  ; (Could also be done as many-to-one, with from-ratio = length from-list)
  (foreach to-list [
    [ann] ->
    ask ann [
      foreach from-list [
        from-node ->
        set ann-sorted-in-an-links fput (
          new-an-link-from (
            from-node
          ) 0 updateable-weights?
        ) ann-sorted-in-an-links
      ]
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to initialize-weights [given-layers-weights-list]
  ; Run as person with pp-net defined.
  (foreach given-layers-weights-list (
    map [n -> first item n pp-net] [2 4 6 8] ; in-links to Features, c-models, predictions, decisions
    ) [
    [list-of-lists-of-weights layer-of-nodes] ->
    ;print list-of-lists-of-weights
    ;print layer-of-nodes
    (foreach list-of-lists-of-weights layer-of-nodes [
      [weights-list ann] ->
      (foreach weights-list ([ann-sorted-in-an-links] of ann) [
        [w anl] ->
        ask anl [set anl-weight w]
      ])
    ])
  ])
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
  report (map [
    n ->
    map [ann -> [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of ann] first item n pp-net
    ] [2 4 6 8]
;    map [dec -> [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of dec] pp-decision
;    map [pred -> [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of pred] pp-predictions
;    map [cm -> [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of cm] pp-c-models
;    map [cm -> [map [fea -> [map [anl -> [anl-weight] of anl] ann-sorted-in-an-links] of [end1] of fea] but-first ann-sorted-in-an-links] of cm] pp-c-models
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to print-ann-weights
  ; run by person
  print ""
  show "Node In-Weights by Layer (Item of PP-Net):"
  print "Layer-ID : [Bias Weight(s)]"
  (foreach pp-weights-list-of-lists [2 4 6 8] [
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
  let cmod-pos [position (in-an-link-from cmod) ann-sorted-in-an-links] of [first pp-predictions] of ann-owner

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

to reposition-all-c-models
  ; run as person

  ; Move all other nodes to be nearer person
  let spcings net-layer-spacings
  reposition-net
  (sentence (sublist pp-net 0 1) (sublist pp-net 5 length pp-net))
  (sentence (sublist spcings 0 1) (sublist spcings 5 length spcings))

  ;tie-to-c-models ; Tie c-model's inputs (features, bias) to it, so they move when it does.
  foreach pp-c-models [
    cm ->
    ask cm [reposition-c-model]
  ]
  ;untie-to-c-models ; Tie c-model's inputs (features, bias) to it, so they move when it does.

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

  ask ego [if pp-learns? [learn-from alter]]
  ask alter [if pp-learns? [learn-from ego]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to choose-action-against [alter]
  ifelse Use-Decision-Nodes? [
    ; Using decision.
    ; Treat its activation as a probability for a Bernoulli-distributed mixed strategy.
    activate-net pp-net [pp-attributes] of alter
    set pp-action [ann-random-bernoulli] of first pp-decision
  ] [
    activate-net pp-net [pp-attributes] of alter ; Include Decisions, just to test mechanism, even if not using them.
    ;activate-net (but-last pp-net) [pp-attributes] of alter ; Exclude decision
    ; Using prediction, skipping decision node and using a fixed rule.
    set pp-action (ifelse-value
      (msne > 100 * [ann-activation] of first pp-predictions) [1]
      (msne < 100 * [ann-activation] of first pp-predictions) [0]
      (ifelse-value (msne > random-float 100) [1] [0])
    )
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to learn-from [given-opponent]
  ; run as person

  ; If not output node:
  ; dC/dai = sum_j ( wij * dC/dzj )
  ; dC/dzj = aj * (1 - aj) * dC/daj
  ; When all errors backpropagated fully, update weights:
  ; wij = wij - alpha * (ai * dC/dzj + lambda * wij)

  ifelse Use-Decision-Nodes? [
    ; Start with Decisions
    set pp-target [1 - pp-action] of given-opponent ; Trying to mis-match opponent's action with one's own decision.
    update-cost
    backpropagate-net (reverse pp-net) (list pp-target)
  ]
  [
    ; Start with Predictions
    set pp-target [pp-action] of given-opponent ; Trying to predict opponent's action
    update-cost
    backpropagate-net (reverse sublist pp-net 0 (-2 + length pp-net)) (list pp-target)
  ]

  ; Having backpropagated errors, use them to compute new weights.
  foreach pp-net [
    lay ->
    ; update-layer-in-weights list-of-nodes learning-rate weight-penalty
    update-layer-in-weights (item 0 lay) (item 2 lay) (item 3 lay)
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
    statistics-retention * pp-recent-cost + ifelse-value use-decision-nodes? [
      (100 - statistics-retention) * sum map [ann -> [ann-cost [pp-target] of ann-owner] of ann] pp-decision
    ] [
      (100 - statistics-retention) * sum map [ann -> [ann-cost [pp-target] of ann-owner] of ann] pp-predictions
    ]
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
  if Label-AN-Nodes? [relabel-all-an-nodes]
  if is-an-node? Selected-AN-Node [do-an-plots]
  ;if monitors? [if is-an-node? Selected-AN-Node [do-an-plots]]
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

to relabel-all-an-nodes
  foreach sort an-nodes [
    ann ->
    ask ann [ relabel-an-node ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to relabel-an-node
  ; Run as an-node
  (ifelse
    (label-nodes-by = "No Label") [set label ""]
    (label-nodes-by = "Who") [set label who]
    (label-nodes-by = "Type-ID") [set label (word ann-type-id " ")]
    (label-nodes-by = "Out-Weights") [
      ifelse 1 = length ann-sorted-out-an-links [
        set label (word ([precision anl-weight 1] of first ann-sorted-out-an-links))
      ]
      [
        ;set label (word (map [anl -> [precision anl-weight 1] of anl] ann-sorted-out-an-links))
        set label ""
      ]
    ]
    (label-nodes-by = "Activation") [set label (word (precision ann-activation 3))]
    (label-nodes-by = "Delta Activation") [set label (word (precision ann-new-delta-activation 3))]
    (label-nodes-by = "z = Sum( Weight * Input )") [
      set label ifelse-value (empty? ann-sorted-in-an-links) [""] [(word (precision ann-sum-of-weighted-inputs 1))]
    ]
    (label-nodes-by = "Delta z") [
      set label ifelse-value (empty? ann-sorted-in-an-links) [""] [(word (precision ann-delta-z 2))]
    ]
    (label-nodes-by = "Last z") [set label (word (precision ann-last-delta-activation 3))]
    (label-nodes-by = "Last Delta Activation") [set label (word (precision ann-last-delta-activation 3))]
    (label-nodes-by = "Last Delta Out-Weight") [
      ifelse 1 = length ann-sorted-out-an-links [
        set label (word ([precision anl-last-delta-weight 1] of first ann-sorted-out-an-links))
      ]
      [
        ;set label (word (map [anl -> [precision anl-last-delta-weight 1] of anl] ann-sorted-out-an-links))
        set label ""
      ]
    ]
  )
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
          set selected-an-node [first pp-predictions] of nearest-turtle
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
  ask first pp-decision [
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
          set selected-an-node [first pp-predictions] of pp
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

to-report weighted-sum [list-of-weights list-of-inputs]
  report sum (map [[w x] -> w * x] list-of-weights list-of-inputs)
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

    if monitors? [ ; If not saving time by skipping live updates
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
                ;fput 1 replace-item k (n-values (-1 + length ann-sorted-in-an-links) [i -> 0]) k
              )
            )
          ]
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to test1
  set Number-Of-People 2
  set sorted-people n-values number-of-people [-> new-person (base-list-of-weights number-of-c-beliefs) true]

  let tmp-ox mean list max-pxcor min-pxcor
  let tmp-oy mean list max-pycor min-pycor
  ask first sorted-people [
    ;set color red
    setxy (-0.4 * world-width + tmp-ox) (0.4 * world-height + tmp-oy)
    reposition-net pp-net net-layer-spacings
  ]
  ask last sorted-people [
    ;set color blue - 1
    setxy (0.4 * world-width + tmp-ox) (-0.4 * world-height + tmp-oy)
    reposition-net pp-net net-layer-spacings
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to test2
  let adaptive-group n-values 1 [-> new-person (base-list-of-weights number-of-c-beliefs) true]
  let group-a n-values (0.25 * (number-of-people - 1)) [-> new-person (base-list-of-weights 0) false]
  let group-b n-values (number-of-people - 1 - length group-a) [-> new-person (base-list-of-weights 0) false]

  foreach adaptive-group [
    pp ->
    ask pp [
      set-hidden-for-ann false
      set color red
      setxy 0 0

      reposition-net pp-net net-layer-spacings
;      if Reposition-C-Models? [reposition-all-c-models]
    ]
  ]

  foreach group-a [
    pp ->
    ask pp [
      set color blue - 1
      setxy (- random-float abs min-pxcor) (random-float max-pycor)
      reposition-net pp-net net-layer-spacings
      set pp-learns? false
      ask [first ann-sorted-in-an-links] of first pp-predictions [
        set anl-weight log-odds 0.001 ; Low expectations of Hawk. Will therefore play Hawk
      ]
    ]
  ]

  foreach group-b [
    pp ->
    ask pp [
      set color lime
      setxy (random-float max-pxcor) (- random-float abs min-pycor)
      reposition-net pp-net net-layer-spacings
      set pp-learns? false
      ask [first ann-sorted-in-an-links] of first pp-predictions [
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

to test3
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
      reposition-net pp-net net-layer-spacings
      if Reposition-C-Models? [reposition-all-c-models]
    ]
  ]

  foreach group-a [
    pp ->
    ask pp [
      set color blue - 1
      setxy (- random-float abs min-pxcor) (random-float max-pycor)
      reposition-net pp-net net-layer-spacings
      set pp-learns? false
      ask [first ann-sorted-in-an-links] of first pp-predictions [
        set anl-weight log-odds 0.001 ; Low expectations of Hawk. Will therefore play Hawk
      ]
    ]
  ]

  foreach group-b [
    pp ->
    ask pp [
      set color lime
      setxy (random-float max-pxcor) (random-float max-pycor)
      reposition-net pp-net net-layer-spacings
      set pp-learns? false
      ask [first ann-sorted-in-an-links] of first pp-predictions [
        set anl-weight log-odds 0.999 ; High expectations of Hawk. Will therefore play Dove
      ]
    ]
  ]

  foreach group-c [
    pp ->
    ask pp [
      set color lime
      setxy (- random-float abs min-pxcor) (- random-float abs min-pycor)
      reposition-net pp-net net-layer-spacings
      set pp-learns? false
      ask [first ann-sorted-in-an-links] of first pp-predictions [
        set anl-weight log-odds 0.999 ; High expectations of Hawk. Will therefore play Dove
      ]
    ]
  ]

  foreach group-d [
    pp ->
    ask pp [
      set color blue - 1
      setxy (random-float max-pxcor) (- random-float abs min-pycor)
      reposition-net pp-net net-layer-spacings
      set pp-learns? false
      ask [first ann-sorted-in-an-links] of first pp-predictions [
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

to-report base-node-layer-list
  report (list
    ; List-Of-Nodes Node-Type Learning-Rate Weight-Penalty
    ; Node-Type is used as a key to tables activations and backpropagations

    (list ; 0 : Inputs
      (n-values number-of-attributes [-> new-an-node 0])
      node-type-input 0 0
    )

    (list ; 1 : Biases to features
      (n-values (number-of-features * Number-Of-C-Beliefs) [-> new-an-node 1])
      node-type-bias 0 0
    )

    (list ; 2 : Features
      (n-values (number-of-features * Number-Of-C-Beliefs) [-> new-an-node 2])
      ;node-type-square inertia lambda
      node-type-sigmoid (100 - inertia) lambda
    )

    (list ; 3 : Biases to C-Models. Omit if using softmax.
      (n-values Number-Of-C-Beliefs [-> new-an-node 3])
      node-type-bias 0 0
    )

    (list ; 4 : C-Models
      (n-values Number-Of-C-Beliefs [-> new-an-node 4])
;      node-type-sigmoid 0 lambda
      node-type-softmax 0 lambda
    )

    (list ; 5 : Bias to Prediction
      (n-values 1 [-> new-an-node 5])
      node-type-bias 0 0
    )

    (list ; 6 : Prediction
      (n-values 1 [-> new-an-node 6])
      (ifelse-value use-decision-nodes? [node-type-sigmoid] [node-type-sigmoid-output]) (100 - memory) lambda
    )

    (list ; 7 : Bias to Decision
      (n-values 1 [-> new-an-node 7])
      node-type-bias 0 0
    )

    (list ; 8 : Decision
      (n-values 1 [-> new-an-node 8])
      node-type-sigmoid-output 0 lambda
    )
  )
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
    ; C-Models' Features:
    ifelse-value (0 = num-c-models) [[]] [reduce sentence (n-values num-c-models [j ->
      features-weights random-coordinates
    ])]

    ; To C-Models:
    (n-values num-c-models [i ->
      (fput (
        6 * (- number-of-features + 0.5)
      ) n-values number-of-features [-> 6]) ; Slope = 6 (steep). All features must be activated to activate C-Model.
    ])

    ; To Prediction:
    (n-values 1 [j ->
      (fput (
        log-odds (0.01 * msne)
      ) n-values num-c-models [i -> random-normal 0 0.01]) ; Default prediction = msne. Even if activated, C-Models are initially neutral.
    ])

    ; To Decision:
    (n-values 1 [j ->
      (list (-0.01 * msne * -25) -25) ; -50 gives steep slope. Aim to deliver activations either close to 0 (pred>msne) or close to 1 (pred<msne)
    ])

  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report features-weights [given-cors]
  let x item 0 given-cors
  let y item 1 given-cors

  report (list
    (list (- (x - Feature-Coverage) * Feature-Slope) Feature-Slope) ; Lower bound for attribute value
    (list ((x + Feature-Coverage) * Feature-Slope) (- Feature-Slope)) ; Upper bound for attribute value
    (list (- (y - Feature-Coverage) * Feature-Slope) Feature-Slope) ; Lower bound for attribute value
    (list ((y + Feature-Coverage) * Feature-Slope) (- Feature-Slope)) ; Upper bound for attribute value
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiler for seeking improvements to computer speed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to do-profile-test
  clear-all
  let start-time timer
  print "\nProfiling..."
  setup                  ;; set up the model
  print (word "\nSetup took " (timer - start-time) " seconds.\n")
;  let start-time timer
  repeat 0 [go]       ;; Simulate Warm-up period
  profiler:start         ;; start profiling
  repeat 1000 [go]       ;; run something you want to measure
  profiler:stop          ;; stop profiling
  print profiler:report  ;; view the results
  profiler:reset         ;; clear the data

  print (word (count people) " people, " (count an-nodes) " AN-Nodes, and " (count an-links) " AN-Links at end.")
  print (word "\nFinished test after " (timer - start-time) " seconds.")
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
0
0
1
-8
8
-8
8
1
1
1
ticks
30.0

INPUTBOX
7
106
159
166
Number-Of-People
2.0
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
2.0
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
1.5042048E8
1
0
Number

INPUTBOX
10
860
162
920
Seed-Go
4598585.0
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
445
595
592
628
Clear AN-Node Labels
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
Reposition Net
foreach sorted-people [\npp ->\nask pp [\nreposition-net pp-net net-layer-spacings\n]\n]
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
270
630
382
663
Hide-ANN?
Hide-ANN?
1
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
Selected-AN-Node
ifelse-value Monitors? [\nSelected-AN-Node\n] [\"\"]
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
ifelse-value Monitors? [\n[ann-type] of selected-an-node\n] [\"\"]
17
1
11

MONITOR
645
155
760
200
ANN-Owner
ifelse-value Monitors? [\n[ann-owner] of selected-an-node\n] [\"\"]
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
ifelse-value Monitors? [\n[map [\nanl -> (word [precision anl-weight 3] of anl \" \")\n] ann-sorted-in-an-links] of Selected-AN-Node\n] [\"\"]
17
1
11

MONITOR
1030
185
1310
230
Input Activations
ifelse-value Monitors? [\n[map [\nanl -> (word [precision ann-activation 3] of [end1] of anl \" \")\n] ann-sorted-in-an-links] of Selected-AN-Node\n] [\"\"]
17
1
11

BUTTON
1140
545
1357
578
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
650
135
710
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
650
250
710
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
"Base-Population" "Test1" "Test2" "Test3"
1

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
645
445
767
478
Recolor by Belief
foreach sorted-people [\npp ->\nask pp [ set color scale-color red (\n[ann-activation] of first pp-predictions\n) -0.2 1.2 ]\n]
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
ifelse-value Monitors? [\n[map [f -> precision (100 * f) 1] pp-freqs] of selected-person\n] [\"\"]
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

BUTTON
350
670
482
703
Print Base Weights
foreach base-list-of-weights 2 [w -> print w]
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
1315
185
1500
230
Input Node Who
ifelse-value Monitors? [\n[map [\nanl -> (word [who] of [end1] of anl \" \")\n] ann-sorted-in-an-links] of Selected-AN-Node\n] [\"\"]
17
1
11

TEXTBOX
450
445
585
463
AN-Node Labels:
13
0.0
1

CHOOSER
445
505
637
550
Label-Nodes-By
Label-Nodes-By
"No Label" "Who" "Type-ID" "Out-Weights" "Activation" "Delta Activation" "z = Sum( Weight * Input )" "Delta z" "Last z" "Last Delta Activation" "Last Delta Out-Weight"
4

SWITCH
445
465
592
498
Label-AN-Nodes?
Label-AN-Nodes?
0
1
-1000

BUTTON
445
555
572
588
Relabel AN-Nodes
relabel-all-an-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
220
595
382
628
Recolor-AN-Nodes?
Recolor-AN-Nodes?
1
1
-1000

MONITOR
990
235
1057
280
Activation
ifelse-value Monitors? [\n[ann-activation] of Selected-AN-Node\n] [\"\"]
3
1
11

MONITOR
1205
235
1332
280
Last Delta-Activation
ifelse-value Monitors? [\n[ann-last-delta-activation] of Selected-AN-Node\n] [\"\"]
4
1
11

MONITOR
1335
235
1410
280
Delta z
ifelse-value Monitors? [\n[ann-delta-z] of Selected-AN-Node\n] [\"\"]
4
1
11

MONITOR
1060
235
1142
280
Sum(w * inp)
ifelse-value Monitors? [\n[ann-sum-of-weighted-inputs] of Selected-AN-Node\n] [\"\"]
3
1
11

SWITCH
10
715
177
748
Use-Decision-Nodes?
Use-Decision-Nodes?
1
1
-1000

MONITOR
1415
235
1562
280
Last Input Delta Weights
ifelse-value Monitors? [\n[map [\nanl -> (word [precision anl-last-delta-weight 3] of anl \" \")\n] ann-sorted-in-an-links] of Selected-AN-Node\n] [\"\"]
17
1
11

MONITOR
1145
235
1202
280
Last z
ifelse-value Monitors? [\n[ann-last-z] of Selected-AN-Node\n] [\"\"]
3
1
11

BUTTON
350
750
462
783
Do-Profile-Test
do-profile-test
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
795
245
902
278
Monitors?
Monitors?
0
1
-1000

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

an-link
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 120 180
Line -7500403 true 150 150 180 180
@#$#@#$#@
1
@#$#@#$#@
