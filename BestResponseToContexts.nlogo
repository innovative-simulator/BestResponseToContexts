;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Play Best Response Given Context-Dependent Beliefs
;; This program (C) Christopher J Watts, 2024.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [array profiler matrix]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [
  sorted-people
  current-person

  world-wraps-h?
  world-wraps-v?

;  payoffs-list
;  msne

  ; Arrays by interaction type
  num-by-i-type
  cur-num-by-i-type

  sorted-people-by-mfi-type ; list of lists
  mfi-component-sizes

  ; Random Number Seeds:
  previous-seed-setup
  previous-seed-go

  ; Testing whether to halt
  moving-sample
  sample-ticks ; Numbers 0 to (-1 + halting-sample-size)
  num-halting-tests-passed

  time-of-shock
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

breed [people person]
breed [c-beliefs c-belief]

directed-link-breed [c-links c-link]
undirected-link-breed [mfi-links mfi-link]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

people-own [
  pp-action ; Chosen action in latest interaction.
  pp-current-c-belief ; Chosen C-Belief in latest interaction.
  pp-num-interactions ; Used with total payoff to calculate mean.
  pp-total-payoff ; Running total of payoffs received.
  pp-freqs ; Frequencies of interactions by MFI type, weighted for recency.
  pp-most-freq-interaction ; ID for recent most frequent type of interaction
  pp-mfi-component ; Component ID in network of links between close people with same MFI type.
  pp-out-cl-neighbors ; Sorted list of C-Beliefs attached to this person.
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

c-beliefs-own [
  cb-degree ; Degree of belief about opponent's Hawk-play in this context.
  cb-date-of-activation ; Could be used to check whether some beliefs are being used more than others?
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

c-links-own [
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  foreach sort patches [pa -> ask pa [set pcolor patch-color]]
  setup-world-wraps
;  set msne current-msne
;  set payoffs-list payoffs
  setup-rng "seed-setup"
  setup-people
  rescale-mfi-network-radius
  reposition-c-beliefs
  set current-person nobody

  reset-ticks
  set time-of-shock Ticks-Until-Shock

  set num-by-i-type n-values 4 [-> 0]

  set sample-ticks n-values halting-sample-size [n -> n]
  set moving-sample n-values 4 [-> n-values halting-sample-size [n -> 0]]
  set num-halting-tests-passed 0

  calc-sorted-people-by-mfi-type
  setup-time-series-plots
  update-time-series-plots-additionals
  setup-mfi-network
  calc-mfi-network-components

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

to setup-world-wraps
  ; Useful, if trying to change xcor and ycor using setxy, and not face, fd, distance
  let d [distance patch max-pxcor max-pycor] of patch min-pxcor min-pycor
  set d precision (d * d) 3
  (ifelse
    (d = 2) [
      set world-wraps-h? true
      set world-wraps-v? true
      stop
    ]
    (d = ((world-width - 1) ^ 2) + ((world-height - 1) ^ 2)) [
      set world-wraps-h? false
      set world-wraps-v? false
      stop
    ]
  )
  set d [distance patch min-pxcor max-pycor] of patch min-pxcor min-pycor
  set d precision (d * d) 3
  if d = 1 [
    set world-wraps-h? false
    set world-wraps-v? true
    stop
  ]
  set world-wraps-h? true
  set world-wraps-v? false

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report random-beta [alpha beta]
  let x random-gamma alpha 1
  report (x / (x + random-gamma beta 1))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report random-beta-reweighted [weight perc]
;  ; Given a percentage returns a beta-distributed number in range [0.001 99.999]
  ; Given a percentage returns a beta-distributed number in range [0 100]
  ; with mean = perc.
  let alpha 0.01 * perc * weight
  let beta 0.01 * (100.0 - perc) * weight
  if alpha <= 0 [print (word "FATAL! Trying to sample random-beta-reweighted, but alpha = " alpha " (perc = " perc ", weight = " weight ").")]
  if beta <= 0 [print (word "FATAL! Trying to sample random-beta-reweighted, but beta = " beta " (perc = " perc ", weight = " weight ").")]
  let x random-gamma alpha 1
  report max (list 0.001 (min (list 99.999 (100 * (x / (x + random-gamma beta 1))))))
;  report max (list 0 (min (list 100 (100 * (x / (x + random-gamma beta 1))))))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report payoffs
  report payoffs-hd
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report payoffs-hd
  report (list
    (0.5 * hd-value)
    0
    hd-value
    ifelse-value (msne = 0) [(- infinity)] [0.5 * (hd-value - (100 * hd-value / msne))]
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report hd-cost-of-conflict
  if 0 = base-msne [report infinity]
  report 100 * HD-Value / base-msne
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report infinity
  ; NB: Arbitrarily large number
  report 2 ^ 31
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report msne-exp-payoff
  report 0.01 * (
    ((100 - base-msne) * (item 0 payoffs)) +
    (base-msne * item 1 payoffs)
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report msne
  report base-msne

  ; What if agents on one side had different MSNE?
;  if not is-turtle? self [
;    report base-msne
;  ]
;  report 100 * (ycor - min-pycor - 0.5) / world-height ; What if just based on ycor?
;  if ycor > 0.0 * max-pycor [report 100 - base-msne] ; What if top half reversed?
;  report base-msne
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report noise-payoff
  report mean payoffs
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report max-payoff
  report max payoffs
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report min-payoff
  report min payoffs
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;to reset-msne
;  if current-msne != msne [
;    set msne current-msne
;    set payoffs-list payoffs
;  ]
;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-people
  set sorted-people []
  repeat number-of-people [
    set sorted-people fput new-person sorted-people
  ]

  set sorted-people reverse sorted-people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report new-person
  let rep-obj nobody

  create-people 1 [
    set hidden? hide-people?
    set rep-obj self
    set shape "person"
    set color grey
    set pp-num-interactions 0
    set pp-total-payoff 0
    set pp-freqs n-values 4 [-> 0]

    setup-person-attributes

    repeat number-of-c-beliefs [
      create-c-link-to new-c-belief [
        set hidden? Hide-C-Links?
        set color [color] of end1
      ]
    ]
    set pp-out-cl-neighbors sort out-c-link-neighbors

;    ; Ensure agent has model relevant to self?
;    ask min-one-of out-c-link-neighbors [who] [
;      move-to myself
;    ]
  ]
  report rep-obj
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-person-attributes
  (ifelse
    Initial-Person-Attributes = "Random" [setxy random-xcor random-ycor]
    Initial-Person-Attributes = "4 Clusters" [setup-person-attributes-four-clusters]
    Initial-Person-Attributes = "Random-Patch" [setup-person-attributes-random-patch]
    Initial-Person-Attributes = "Square Grid" [setup-person-attributes-square-grid]
    Initial-Person-Attributes = "Triangular Grid" [setup-person-attributes-triangular-grid]
    Initial-Person-Attributes = "Gaussian" [setup-person-attributes-gaussian]
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-person-attributes-four-clusters
    let clust-weight (clustering-weight + 1) ; Turn weight into alpha and beta parameters
    setxy (
      (ifelse-value (100 * count people < Perc-Right-Hand-Side * Number-Of-People) [1] [-1]) *
      world-width * 0.5 * random-beta clust-weight clust-weight
    ) (
      (ifelse-value (1 = (count people) mod 2) [1] [-1]) *
      world-height * 0.5 * random-beta clust-weight clust-weight
    )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-person-attributes-random-patch
  ; Needs count patches >= number-of-people
  ; May be painfully slow
  move-to one-of patches with [not any? people-here]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-person-attributes-square-grid
  let num-cols ceiling sqrt Number-Of-People
  let num-rows ceiling (Number-Of-People / num-cols)
  let cur-item -1 + count people
  setxy (world-width * (cur-item mod num-cols) / num-cols) (world-height * (int (cur-item / num-cols)) / num-rows)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-person-attributes-triangular-grid
  let num-cols ceiling sqrt Number-Of-People
  let num-rows ceiling (Number-Of-People / num-cols)
  let cur-item -1 + count people
  setxy (world-width * ((ifelse-value (0 = (int (cur-item / num-cols)) mod 2) [0] [0.5]) + cur-item mod num-cols) / num-cols) (world-height * (int (cur-item / num-cols)) / num-rows)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-person-attributes-gaussian
  let mult 0.25
  ;let mult 1 / 6

  let x random-normal (mean list min-pxcor max-pxcor) (mult * (-1 + world-width))
  while [x > max-pxcor or x < min-pxcor] [
    set x random-normal (mean list min-pxcor max-pxcor) (mult * (-1 + world-width))
  ]

  let y random-normal (mean list min-pycor max-pycor) (mult * (-1 + world-height))
  while [y > max-pycor or y < min-pycor] [
    set y random-normal (mean list min-pycor max-pycor) (mult * (-1 + world-height))
  ]

  setxy x y
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reposition-c-beliefs
  (ifelse
    (Initial-C-Belief-Positions = "Random") [] ; Default already
    (Initial-C-Belief-Positions = "Ring Around (0, 0)") [homogenous-ringed-c-beliefs (min (list world-width world-height)) / 3]
    (Initial-C-Belief-Positions = "Ring Around Agent") [heterogeneous-ringed-c-beliefs (min (list world-width world-height)) / 3]
    (Initial-C-Belief-Positions = "At Agent") [heterogeneous-ringed-c-beliefs 0]
    (Initial-C-Belief-Positions = "At Other Agents") [match-c-beliefs-to-people]
    [user-message "WARNING! Could not identify procedure for Initial-C-Beliefs"]
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to match-c-beliefs-to-people
  foreach sorted-people [ego ->
    ask ego [
      let num-to-match min (list (count out-c-link-neighbors) (length sorted-people))
      (foreach (sort n-of num-to-match out-c-link-neighbors) (n-of num-to-match sorted-people) [[cb alter] ->
        ask cb [move-to alter]
        ]
      )
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to homogenous-ringed-c-beliefs [given-radius]
  foreach sorted-people [ego ->
    ask ego [
      layout-circle pp-out-cl-neighbors given-radius
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to heterogeneous-ringed-c-beliefs [given-radius]
  foreach sorted-people [ego ->
    ask ego [
      ifelse 1 = count out-c-link-neighbors [
        foreach pp-out-cl-neighbors [cb ->
          ask cb [move-to myself]
        ]
      ] [
        let a 360 / count out-c-link-neighbors
        (foreach pp-out-cl-neighbors (n-values (length pp-out-cl-neighbors) [k -> k]) [[cb k] ->
          ask cb [
            move-to myself
            set heading a * k
            fd given-radius
          ]
        ])
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report new-c-belief
  let rep-obj nobody
  hatch-c-beliefs 1 [
    set hidden? Hide-C-Beliefs?
    set rep-obj self
    set shape "circle"
    set size 0.5
    setxy random-xcor random-ycor
    set cb-degree cb-initial-degree
    set cb-date-of-activation -1
  ]
  report rep-obj
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report cb-initial-degree
  report runresult initial-degree-of-belief
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report Bernoulli [given-perc]
  ; Bernoulli random
  report ifelse-value (given-perc > random-float 100) [100] [0]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to highlight-current-person
  if mouse-inside? [
    if mouse-down? [
      let candidates [people-here] of mouse-patch
      Ifelse not any? candidates [
        if is-person? current-person [
          ask current-person [unhighlight]
          set current-person nobody
        ]
      ]
      [
        if is-person? current-person [
          ask current-person [unhighlight]
        ]
        set current-person one-of candidates
        ask current-person [highlight]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to move-current-person
  if mouse-inside? [
    ifelse mouse-down? [
      ifelse is-person? current-person [
        if mouse-patch != [patch-here] of current-person [
          ask current-person [move-to mouse-patch]
        ]
      ]
      [
        let candidates [people-here] of mouse-patch
        If any? candidates [
          set current-person one-of candidates
        ]
      ]
    ]
    [
      set current-person nobody
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to highlight
  set size 2
  foreach sort my-out-c-links [cl ->
    ask cl [
      set hidden? false
      set thickness 0.5
      set color [color] of end1
      ask end2 [
        set hidden? false
        set size 2 * size
      ]
    ]
  ]
  foreach sort patches [pa ->
    ask pa [
      set pcolor [color] of [min-one-of (out-c-link-neighbors with-min [distance pa]) [who]] of myself
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to unhighlight
  set size 1
  set label ""
  foreach sort my-out-c-links [cl ->
    ask cl [
      set hidden? true
      set thickness 0
      ask end2 [
        set hidden? true
        set size 0.5 * size
      ]
    ]
  ]
  foreach sort patches [pa ->
    ask pa [set pcolor patch-color]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report mouse-patch
  report patch (mouse-xcor) (mouse-ycor)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  if ticks = run-length [stop]
  if halting-after-tests? [stop]

  ;  reset-msne
  set cur-num-by-i-type array:from-list n-values 4 [-> 0]

  let pairs shuffle sorted-people
  set pairs (list
    sublist pairs 0 (int (0.5 * length pairs))
    sublist pairs (int (0.5 * length pairs)) ((length pairs) + ifelse-value (0 = (length pairs) mod 2) [0] [-1])
  )

  (foreach (first pairs) (last pairs) [[ego alter] ->
    ask ego [choose-action-against alter]
    ask alter [choose-action-against ego]
    ask ego [
      update-degree-of-belief-against alter
      update-relevance-against alter
      update-pp-stats [pp-action] of alter
      run recolor-person-method
    ]
    ask alter [
      update-degree-of-belief-against ego
      update-relevance-against ego
      update-pp-stats [pp-action] of ego
      run recolor-person-method
    ]

    let i-type (2 * [pp-action] of ego) + [pp-action] of alter
    array:set cur-num-by-i-type i-type 1 + array:item cur-num-by-i-type i-type
  ])

  tick

  set num-by-i-type map [a -> a] array:to-list cur-num-by-i-type ; This is been fine.
;  set num-by-i-type (map [[a b] -> ; This was a mistake. Smoothing overkill. Makes no difference to stats from converged models though.
;    0.01 * (
;      statistics-retention * a +
;      (100 - statistics-retention) * b
;    )
;  ] num-by-i-type (array:to-list cur-num-by-i-type))

  calc-sorted-people-by-mfi-type

  ; Update moving sample
  foreach n-values 4 [mfi -> mfi] [mfi ->
    set moving-sample replace-item mfi moving-sample (
      replace-item (ticks mod halting-sample-size) (item mfi moving-sample) (perc-interaction-type mfi)
    )
  ]

  update-time-series-plots
  if 0 = ticks mod Ticks-Between-Plot-Updates [
    ; Less interesting, so updated less frequently
    update-time-series-plots-additionals
  ]
  if 0 = ticks mod Ticks-Between-Network-Updates [
    setup-mfi-network
    calc-mfi-network-components
  ]

  if ticks = time-of-shock [
    simulate-shock
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report halting-after-tests?
  if Halt-On-Tests? [
    if ticks > halting-sample-size [
      if 0 = ticks mod halting-sample-size [ ; Time to do another test
        let latest-item ticks mod halting-sample-size
        foreach n-values 4 [mfi -> mfi] [mfi ->
          if halting-slope-bound < abs (item 1 first matrix:regress matrix:from-column-list (list
            (
              sentence
              (sublist (item mfi moving-sample) (latest-item + 1) halting-sample-size)
              (sublist (item mfi moving-sample) 0 (latest-item + 1))
          )
          sample-ticks
          )) [
            set num-halting-tests-passed 0
            report false
          ]
        ]
        ; Passed test for all mfi types!
        set num-halting-tests-passed num-halting-tests-passed + 1
        ; Did we pass enough times in a row?
        report (num-halting-tests-passed >= Halting-Tests-Required)
      ]
    ]
  ]
  report false
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to do-final-stats
  ; Useful for BehaviorSpace
  ; Should make sense in the light of Go above.
  calc-sorted-people-by-mfi-type
  setup-mfi-network
  calc-mfi-network-components
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report best-c-belief-against [given-opponent]
  report min-one-of out-c-link-neighbors [distance given-opponent]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report best-action-against [given-opponent]
  report best-response [cb-degree] of best-c-belief-against given-opponent
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to choose-action-against [given-opponent]
  set pp-current-c-belief best-c-belief-against given-opponent
  ;set pp-action best-response [cb-degree] of pp-current-c-belief
  set pp-action chosen-action [cb-degree] of pp-current-c-belief
  if epsilon > 0 [ ; Chance of exploring alternative actions?
    if epsilon > random-float 100 [
      set pp-action 1 - pp-action
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report chosen-action [given-belief]
  if Response-Choice = "Best" [report best-response given-belief]
  if Response-Choice = "Stochastic" [report stochastic-response given-belief]
  if Response-Choice = "MSNE" [report best-response base-msne]

  report best-response given-belief

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-degree-of-belief-against [given-opponent]
  ask pp-current-c-belief [
    set cb-degree 0.01 * (
      ((100 - memory) * 100 * [pp-action] of given-opponent) +
      (memory * cb-degree)
    )
    c-beliefs-recolor
    set cb-date-of-activation ticks
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-relevance-against [given-opponent]
  ; Depending outcome (mis-matching actions?) of interaction,
  ; make current C-Belief more or less relevant for dealing with given-opponent
  ; Relevance here is based on spatial proximity.

  ; Remember: HD is a *mis-matching* game! Score 1 for a mis-match, 0 for a match
  ;let relevance-correction 1 ; Effectively what we ran before.
  let relevance-correction -1 + 2 * abs (pp-action - [pp-action] of given-opponent) ; Move towards those I mis-match, away from those I match.
  ;let relevance-correction 1 + -2 * abs (pp-action - [pp-action] of given-opponent) ; Wrong way! Move towards those I match, away from those I mis-match.
  ;let relevance-correction (-1 + abs (pp-action - [pp-action] of given-opponent)) ; Move away from those I match. (Surprisingly, no dominance emerges)
  ;let relevance-correction ( abs (pp-action - [pp-action] of given-opponent)) ; Move towards those I mis-match. (Faster dominance emergence)

  ask pp-current-c-belief [
    ; Works whether world wraps or not:
    face given-opponent
    fd (0.01 * (100 - inertia) * relevance-correction * (distance given-opponent))

    ; More complicated. and slower! (But seems to match the above in effect.)
;    setxy (cb-next-xcor xcor ([xcor] of given-opponent) relevance-correction) (cb-next-ycor ycor ([ycor] of given-opponent) relevance-correction)
    ; Will be useful if attribute-space has more than 2 dimensions.

  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report cb-next-xcor [a-cor b-cor relevance-sign]
  ; Take a jump from a-cor to b-cor in the shortest direction (depending on world-wraps-_?)
  if world-wraps-h? [
    report wrapped-xcor (
      a-cor +
      relevance-sign * 0.01 * (100 - inertia) * shortest-jump a-cor b-cor world-width
    )
  ]
  report a-cor + relevance-sign * 0.01 * (100 - inertia) * (b-cor - a-cor)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report cb-next-ycor [a-cor b-cor relevance-sign]
  ; As cb-next-xcor, but with world-wraps-v?, wrapped-ycor, world-height
  if world-wraps-v? [
    report wrapped-ycor (
      a-cor +
      relevance-sign * 0.01 * (100 - inertia) * shortest-jump a-cor b-cor world-height
    )
  ]
  report a-cor + relevance-sign * 0.01 * (100 - inertia) * (b-cor - a-cor)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report wrapped-xcor [given-xcor]
  if (given-xcor < min-pxcor - 0.5) [report (given-xcor + world-width)]
  if (given-xcor >= max-pxcor + 0.5) [report (given-xcor - world-width)]
  report given-xcor
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report wrapped-ycor [given-ycor]
  ; As wrapped-xcor, but with min-pycor, max-pycor, world-height
  if (given-ycor < min-pycor - 0.5) [report (given-ycor + world-height)]
  if (given-ycor >= max-pycor + 0.5) [report (given-ycor - world-height)]
  report given-ycor
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report shortest-jump [from-cor to-cor world-size]
  ifelse (to-cor > from-cor) [
    ifelse (2 * (to-cor - from-cor) < world-size) [
      report to-cor - from-cor
    ] [
      report to-cor - from-cor - world-size
    ]
  ] [
    ifelse (2 * (from-cor - to-cor) > world-size) [
      report world-size + to-cor - from-cor
    ] [
      report to-cor - from-cor
    ]
  ]
  report false ; Should not happen.
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-pp-stats [opp-action]
  set pp-num-interactions 1 + pp-num-interactions
  let cur-outcome (opp-action + 2 * pp-action)
  set pp-total-payoff 0.01 * (Statistics-Retention * pp-total-payoff + (100 - Statistics-Retention) * item cur-outcome payoffs)
  ;set pp-total-payoff pp-total-payoff + item cur-outcome payoffs ; payoffs-list
  set pp-freqs map [f -> 0.01 * f * Statistics-Retention] pp-freqs
  set pp-freqs replace-item cur-outcome pp-freqs ((0.01 * (100 - Statistics-Retention)) + item cur-outcome pp-freqs)
  set pp-most-freq-interaction pp-most-freq-interaction-calculated
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report best-response [given-belief]
  (ifelse
    (msne > given-belief) [report 1]
    (msne < given-belief) [report 0]
  )
  report ifelse-value (msne > random-float 100) [1] [0]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report stochastic-response [given-belief]
  report ifelse-value ((renorm-belief given-belief) > random-float 100) [0] [1]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report logistic [x]
  report 1.0 / (1 + exp (- x))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report renorm-belief [given-belief]
  report 100 * (last normalisation-constants) * (
    (logistic (0.01 * Slope-Multiplier * (given-belief - msne)))
    - first normalisation-constants
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report normalisation-constants
  let a (logistic (0.01 * Slope-Multiplier * (0 - msne)))
  let b (logistic (0.01 * Slope-Multiplier * (100 - msne)))
  report list a (1.0 / (b - a))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to c-beliefs-recolor
  if Color-C-Beliefs-By = "Degree" [c-beliefs-recolor-by-belief stop]
  if Color-C-Beliefs-By = "Time Between Activations" [c-beliefs-recolor-by-time stop]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to c-beliefs-recolor-by-belief
  ;set color (color mod 10) + 0.1 * cb-degree
  (ifelse
    (cb-degree > msne + MSNE-Margin) [set color Hawk-Color]
    (cb-degree < msne - MSNE-Margin) [set color Dove-Color]
    [set color MSNE-Color]
  )
  ask my-in-c-links [set color [color] of end1]
  ;ask my-c-links [set color [color] of myself]
  ;set color (color mod 10) + 0.1 * mean [cb-degree] of out-c-link-neighbors
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to c-beliefs-recolor-by-time
  let time-passed ticks - cb-date-of-activation
  ; If k beliefs per person, and (n-1) potential opponents,
  ; expect to be used (n-1) / k.
  ; Expect to be half way between last use and next use.

  let exp-time-passed 0.5 * [count my-out-c-links] of myself ; Assuming c-belief called by person

  (ifelse
    (time-passed <= exp-time-passed) [set color violet + 3]
    (time-passed <= 4 * exp-time-passed) [set color violet ]
    [set color violet - 2]
  )
  ask my-in-c-links [set color [color] of end1]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-c-beliefs
  foreach sorted-people [pp ->
    ask pp [
      foreach pp-out-cl-neighbors [cl ->
        ask cl [c-beliefs-recolor]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-people
  foreach sorted-people [a ->
    ask a [run recolor-person-method]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report recolor-person-method
  report item recolor-person-method-id (list
    [-> person-recolor-by-action]
    [-> person-recolor-by-most-freq-interaction]
    [-> person-recolor-by-payoff]
    [-> person-recolor-by-hawkishness]
    )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report recolor-person-method-id
  report position color-people-by (list
    "Action"
    "Most Frequent Interaction"
    "Payoff"
    "Hawkishness"
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to person-recolor-by-action
  set color ifelse-value (0 = pp-action) [Dove-Color] [Hawk-Color]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to person-recolor-by-most-freq-interaction
  set color item pp-most-freq-interaction interaction-colors
  set shape item pp-most-freq-interaction interaction-shapes
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to person-recolor-by-payoff
  set color scale-color orange (pp-mean-payoff) min-payoff max-payoff
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to person-recolor-by-hawkishness
  ;set color scale-color violet (mean map [alter -> best-response [cb-degree] of best-c-belief-against alter] sorted-people) 1.2 -0.2
  set color item (
    0.01 * (-1 + length hawkishness-colors) * pp-hawkishness
  ) hawkishness-colors
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-hawkishness
  ; What proportion of population would I play "Hawk" against?
  ; NB: Will use random numbers whenever C-Beliefs tie, or degree = MSNE.
  ; NB: Slow to compute, so use it sparingly (e.g. at end of simulation run)
  report 100 * mean map [alter -> best-response [cb-degree] of best-c-belief-against alter] sorted-people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report hawkishness-colors
  report (list (turquoise - 3) grey (orange ) (orange + 1))
;  report (list (orange - 1) grey (turquoise + 1) (turquoise - 2))
;  report (list (yellow - 1) green sky (blue - 1))
;  report (list (lime) grey pink red)
;  report (list sky lime pink red)
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

to-report pp-mean-payoff
  if pp-num-interactions = 0 [report 0]
  report pp-total-payoff / pp-num-interactions
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-most-freq-interaction-calculated
  let max-val max pp-freqs
  report one-of filter [a -> max-val = item a pp-freqs] [0 1 2 3]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-patches-by-people
  foreach sort patches [pa ->
    ask pa [
      recolor-patch-by-people
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-patches-by-c-beliefs
  foreach sort patches [pa ->
    ask pa [
      recolor-patch-by-c-beliefs
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-patch-by-people
  if any? people-here [
    set pcolor 4 + one-of modes [color] of people-here
    stop
  ]
  if any? neighbors with [any? people-here] [
    set pcolor 4 + one-of modes reduce sentence map [pp -> [color] of pp] [people-here] of neighbors
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-patch-by-c-beliefs
  if any? c-beliefs-here [
    set pcolor -3 + one-of modes [color] of c-beliefs-here
    stop
  ]
  if any? neighbors with [any? c-beliefs-here] [
    set pcolor -3 + one-of modes reduce sentence map [cb -> [color] of cb] [c-beliefs-here] of neighbors
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to recolor-patches-by-distance-to-c-beliefs [min-max?]
  let sorted-patches sort patches
  let cbdists map [pa -> [mean [distance myself] of c-beliefs] of pa] sorted-patches
  let min-cbd min cbdists
  let max-cbd max cbdists
  ifelse min-max? [
    (foreach sorted-patches cbdists [[pa dist] ->
      ask pa [ set pcolor scale-color turquoise dist min-cbd max-cbd ]
    ])
  ]
  [
    (foreach sorted-patches cbdists [[pa dist] ->
      ask pa [ set pcolor scale-color pink dist max-cbd min-cbd ]
    ])
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to simulate-shock
  run Shock-Code
  if Shock-Repeats? [
    set time-of-shock ticks + Ticks-Until-Shock
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-time-series-plots
  set-current-plot "Actions"
  clear-plot
  set-current-plot-pen "MSNE"
  plotxy ticks base-msne
  create-temporary-plot-pen "Dove"
  set-plot-pen-color dove-color
  set-plot-pen-mode 1
  set-plot-pen-interval ticks-between-plot-updates
  create-temporary-plot-pen "Hawk"
  set-plot-pen-color hawk-color
  set-plot-pen-mode 1
  set-plot-pen-interval ticks-between-plot-updates


  set-current-plot "Best Responses"
  clear-plot
  set-current-plot-pen "MSNE"
  plotxy ticks base-msne
  create-temporary-plot-pen "Dove"
  set-plot-pen-color dove-color
  set-plot-pen-mode 1
  set-plot-pen-interval ticks-between-plot-updates
  create-temporary-plot-pen "MSNE +/-"
  set-plot-pen-color msne-color
  set-plot-pen-mode 1
  set-plot-pen-interval ticks-between-plot-updates
  create-temporary-plot-pen "Hawk"
  set-plot-pen-color hawk-color
  set-plot-pen-mode 1
  set-plot-pen-interval ticks-between-plot-updates

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-time-series-plots
  set-current-plot "Most Frequent Interaction"
  set-current-plot-pen "DD: Peace"
  plotxy ticks perc-interaction-type 0
  set-current-plot-pen "DH: Dominated"
  plotxy ticks perc-interaction-type 1
  set-current-plot-pen "HD: Dominant"
  plotxy ticks perc-interaction-type 2
  set-current-plot-pen "HH: Conflict"
  plotxy ticks perc-interaction-type 3

  set-current-plot "Interaction Events"
  set-current-plot-pen "DD: Peace"
  plotxy ticks perc-events-dd
  set-current-plot-pen "Dominance"
  plotxy ticks perc-events-dhhd
  set-current-plot-pen "HH: Conflict"
  plotxy ticks perc-events-hh
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-time-series-plots-additionals
  ; Less interesting(?) plots to be updated less frequently(?)

  set-current-plot "Inter-C-Belief-Distance by MFI"
  set-current-plot-pen "DD: Peace"
  if 0 < perc-interaction-type 0 [
    plotxy ticks mean-icb-distance-by-mfi 0
  ]
  set-current-plot-pen "DH: Dominated"
  if 0 < perc-interaction-type 1 [
    plotxy ticks mean-icb-distance-by-mfi 1
  ]
  set-current-plot-pen "HD: Dominant"
  if 0 < perc-interaction-type 2 [
    plotxy ticks mean-icb-distance-by-mfi 2
  ]
  set-current-plot-pen "HH: Conflict"
  if 0 < perc-interaction-type 3 [
    plotxy ticks mean-icb-distance-by-mfi 3
  ]

  set-current-plot "Recent Payoff by MFI"
  set-current-plot-pen "DD: Peace"
  plotxy ticks mean-payoff-by-mfi 0
  set-current-plot-pen "DH: Dominated"
  plotxy ticks mean-payoff-by-mfi 1
  set-current-plot-pen "HD: Dominant"
  plotxy ticks mean-payoff-by-mfi 2
  set-current-plot-pen "HH: Conflict"
  plotxy ticks mean-payoff-by-mfi 3
  set-current-plot-pen "Noise"
  plotxy ticks noise-payoff
  set-current-plot-pen "Exp = MSNE"
  ;set-plot-pen-color ifelse-value (0 = ticks mod (2 * Ticks-Between-Plot-Updates)) [violet] [white]
  plotxy ticks msne-exp-payoff

  set-current-plot "Self-Hawkishness by MFI"
  (foreach mfi-type-labels (n-values (length mfi-type-labels) [n -> n]) [[lab mfi] ->
    set-current-plot-pen lab
    plotxy ticks perc-self-hawkish-mfi-type mfi
  ])

  set-current-plot "Actions"
  let cur-val 0
  set-current-plot-pen "Hawk"
  set cur-val perc-played-hawk
  plotxy ticks cur-val
  set-current-plot-pen "Dove"
  set cur-val cur-val + perc-played-dove
  plotxy ticks cur-val
  set-current-plot-pen "MSNE"
  plotxy ticks base-msne

  set-current-plot "Best Responses"
  set cur-val 0
  set-current-plot-pen "Hawk"
  set cur-val cur-val + perc-best-response-hawk
  plotxy ticks cur-val
  set-current-plot-pen "MSNE +/-"
  set cur-val cur-val + perc-best-response-msne
  plotxy ticks cur-val
  set-current-plot-pen "Dove"
  set cur-val cur-val + perc-best-response-dove
  plotxy ticks cur-val
  set-current-plot-pen "MSNE"
  plotxy ticks base-msne

  set-current-plot "Beliefs Histogram"
  clear-plot
  let cur-interval 5
  set cur-val 0
  let max-val 0
  set-current-plot-pen "Data"
  set-plot-pen-interval cur-interval
  foreach n-values (int (100 / cur-interval)) [n -> n * cur-interval] [x ->
    set cur-val perc-beliefs-between x (x + cur-interval)
    plotxy x cur-val
    if cur-val > max-val [set max-val cur-val]
  ]
  set-current-plot-pen "MSNE"
  plotxy msne 0
  plotxy msne 1.25 * max-val
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-events-hh
  report 200 * (item 3 num-by-i-type) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-events-dd
  report 200 * (item 0 num-by-i-type) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-events-dhhd
  report 200 * (item 1 num-by-i-type + item 2 num-by-i-type) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-played-hawk
  report 100 * (count people with [pp-action = 1]) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-played-dove
  report 100 * (count people with [pp-action = 0]) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-best-response-dove
  report 100 * (count c-beliefs with [cb-degree > msne + msne-margin]) / count c-beliefs
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-best-response-msne
  report 100 * (count c-beliefs with [(cb-degree <= msne + msne-margin) and (cb-degree >= msne - msne-margin)]) / count c-beliefs
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-best-response-hawk
  report 100 * (count c-beliefs with [cb-degree < msne - msne-margin]) / count c-beliefs
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-beliefs-between [lower-b upper-b]
  report 100 * (count c-beliefs with [(cb-degree < upper-b) and (cb-degree >= lower-b)]) / count c-beliefs
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-always-hawk?
  ; Would person play Hawk in every context?
  report not any? out-c-link-neighbors with [cb-degree >= msne]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-always-dove?
  ; Would person play Dove in every context?
  report not any? out-c-link-neighbors with [cb-degree <= msne]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-play-mixture?
  ; Would person play Dove in every context?
  report ifelse-value pp-always-hawk? [false] [not pp-always-dove?]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-always-hawk
  report 100 * (count people with [pp-always-hawk?]) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-always-dove
  report 100 * (count people with [pp-always-dove?]) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-play-mixture
  report 100 * (count people with [pp-play-mixture?]) / count people
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-interaction-type [given-type]
  report 100 * (length item given-type sorted-people-by-mfi-type) / length sorted-people
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

to-report mean-payoff-by-mfi [given-mfi-type]
  if empty? item given-mfi-type sorted-people-by-mfi-type [report mean payoffs]
;  report mean map [pp -> [ifelse-value (pp-num-interactions = 0) [mean payoffs] [pp-total-payoff / pp-num-interactions]] of pp] item given-mfi-type sorted-people-by-mfi-type
  report mean map [pp -> [ifelse-value (pp-num-interactions = 0) [mean payoffs] [pp-total-payoff]] of pp] item given-mfi-type sorted-people-by-mfi-type
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-inter-c-belief-distance
  report mean map [cb-a -> [
    mean map [cb-b -> [distance cb-b] of cb-a] [pp-out-cl-neighbors] of myself
  ] of cb-a] pp-out-cl-neighbors
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report mean-icb-distance-by-mfi [given-mfi-type]
  if empty? item given-mfi-type sorted-people-by-mfi-type [report 0]
;  report mean map [pp -> [ifelse-value (pp-num-interactions = 0) [mean payoffs] [pp-total-payoff / pp-num-interactions]] of pp] item given-mfi-type sorted-people-by-mfi-type
  report (
    100 *
    mean map [pp ->
      [
        ifelse-value (pp-num-interactions = 0) [0] [pp-inter-c-belief-distance]
      ] of pp
    ] item given-mfi-type sorted-people-by-mfi-type
  ) / perc-world-max-distance
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-world-max-distance
  report 0.5 * sqrt ((world-width ^ 2) + (world-height ^ 2))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to do-histogram
  set-current-plot "Histogram"
  clear-plot
  let hdata runresult histogram-values
  set-plot-x-range 0 (1 + max hdata)
  histogram hdata
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link to other people within a given radius if they share your MFI
;; MFI Network has no impact on game and learning mechanisms
;; It is for visualisation purposes only.

to rescale-mfi-network-radius
  ; Run this if you change population size.
  ; Otherwise, network density will vary too much.
  set MFI-Network-Radius 4 * sqrt (196 / number-of-people)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-mfi-network
  ; Link agents with same most-frequent-interaction type if within a given radius of each other
  ask mfi-links [die]

  foreach sorted-people [ego ->
    ask ego [
      ask pp-mfi-neighbors [
        create-mfi-link-with myself [
          set hidden? Hide-MFI-Links?
          set color [color] of end1
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report pp-mfi-neighbors
  report (other people with [
    pp-most-freq-interaction = [pp-most-freq-interaction] of myself
  ]) in-radius mfi-network-radius
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calc-mfi-network-components
  set mfi-component-sizes []
  foreach sorted-people [pp ->
    ask pp [set pp-mfi-component 0]
  ]

  let cur-mfi -1
  let num-comps 0
  let cur-comp-size 0
  let node-stack []
  foreach sorted-people [pp ->
    ask pp [
      if 0 = pp-mfi-component [
        set node-stack fput self node-stack
        if cur-comp-size > 0 [
          set mfi-component-sizes fput (list cur-comp-size cur-mfi) mfi-component-sizes
        ]
        set cur-comp-size 0
        set num-comps 1 + num-comps
        set cur-mfi pp-most-freq-interaction
      ]
    ]

    while [not empty? node-stack] [
      let ego first node-stack
      set node-stack but-first node-stack
      ask ego [
        if 0 = pp-mfi-component [
          set pp-mfi-component num-comps
          set cur-comp-size 1 + cur-comp-size
          set pp-mfi-component num-comps
          foreach sort (mfi-link-neighbors with [pp-mfi-component = 0]) [alter ->
          ;foreach sort (pp-mfi-neighbors with [pp-mfi-component = 0]) [alter ->
            set node-stack fput alter node-stack
          ]
        ]
      ]
    ]
  ]

  set mfi-component-sizes fput (list cur-comp-size cur-mfi) mfi-component-sizes
  set mfi-component-sizes sort-by [[a b] -> first a > first b] mfi-component-sizes
  plot-mfi-components
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to plot-mfi-components
  set-current-plot "MFI Network Components"
  clear-plot
  let mfi-pen-names mfi-type-labels
  let cur-rank 0
  (foreach mfi-component-sizes (n-values (length mfi-component-sizes) [r -> r]) [[smfi r] ->
    set-current-plot-pen item (last smfi) mfi-pen-names
    ;set-plot-pen-color item (last smfi) interaction-colors
    plotxy r first smfi
  ])

  set-current-plot "MFI Component Counts"
  set-current-plot-pen "All"
  plotxy ticks length mfi-component-sizes
  set-current-plot-pen "Size > 1"
  plotxy ticks num-mfi-components-larger-than 1
  set-current-plot-pen "Size > 5"
  plotxy ticks num-mfi-components-larger-than 5

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report num-mfi-components-larger-than [given-size]
  report length filter [smfi -> given-size < first smfi] mfi-component-sizes
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report people-per-mfi-circle
  report pi * (mfi-network-radius ^ 2) * Number-Of-People / count patches
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to print-inter-type-hawkishness
  print "\nFor what % of matches against agents of column type "
  print "do agents of row type play \"Hawk\"?"

  print (word "\n \t" map [t -> (word t " \t")] mfi-type-labels)
  foreach [0 1 2 3] [type-a ->
    let list-of-type-a item type-a sorted-people-by-mfi-type
    print (word (item type-a mfi-type-labels) " \t" (map [type-b ->
      ifelse-value (empty? item type-a sorted-people-by-mfi-type) ["- \t"] [
        ifelse-value (empty? item type-b sorted-people-by-mfi-type) ["- \t"] [
          (word
            round (
              100 * mean map [ego ->
                [
                  mean map [alter -> best-action-against alter] item type-b sorted-people-by-mfi-type
                ] of ego
              ] list-of-type-a
            )
            " \t")
        ]
      ]
      ] [0 1 2 3])
    )
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report perc-self-hawkish-mfi-type [given-type]
  if empty? item given-type sorted-people-by-mfi-type [report msne]
  report 100 * mean map [a -> [best-action-against self] of a] item given-type sorted-people-by-mfi-type
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report mfi-type-labels
  report (list "DD" "DH" "HD" "HH")
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-demo-seeds
  set seed-setup -1925572583
  set seed-go 462326508

  ;set seed-setup 1735661492
  ;set seed-go -1856694694
end

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
  repeat 2000 [go]       ;; run something you want to measure
  profiler:stop          ;; stop profiling
  print profiler:report  ;; view the results
  profiler:reset         ;; clear the data

  print (word (count people) " people, " (count c-beliefs) " c-beliefs, and " (count mfi-links) " MFI-Links at end.")
  print (word "\nFinished test after " (timer - start-time) " seconds.")
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
360
10
797
448
-1
-1
13.0
1
10
1
1
1
0
1
1
1
0
32
0
32
0
0
1
ticks
30.0

INPUTBOX
15
90
167
150
Number-Of-People
200.0
1
0
Number

SLIDER
15
155
232
188
Number-Of-C-Beliefs
Number-Of-C-Beliefs
1
number-of-people
4.0
1
1
Per Person
HORIZONTAL

SLIDER
15
500
187
533
Base-MSNE
Base-MSNE
0
125
90.0
5
1
%
HORIZONTAL

BUTTON
215
70
277
103
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
15
295
187
328
Memory
Memory
0
100
90.0
5
1
%
HORIZONTAL

BUTTON
215
110
278
143
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
205
620
357
680
Run-Length
2000.0
1
0
Number

TEXTBOX
10
10
250
60
Play Best Response Given Context-Dependent Beliefs
20
0.0
1

TEXTBOX
10
65
160
83
(C) Christopher J Watts, 2024.
11
0.0
1

BUTTON
405
455
547
488
Hide / Unhide People
foreach sorted-people [pp ->\nask pp [set hidden? not hidden?]\n]
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
405
525
552
558
Hide / Unhide C-Links
foreach sorted-people [pp ->\nask pp [\nforeach sort my-c-links [cl -> ask cl [\nset hidden? not hidden?\n]]\n]\n]
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
400
940
540
973
Hide-C-Links?
Hide-C-Links?
0
1
-1000

BUTTON
605
555
782
588
Recolor Patches by People
recolor-patches-by-people
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
405
490
562
523
Hide / Unhide C-Beliefs
foreach sorted-people [pp ->\nask pp [\nforeach sort my-c-links [cl -> ask cl [\nask end2 [set hidden? not hidden?]\n]]\n]\n]
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
605
695
762
728
Patches to Patch-Color
foreach sort patches [pa ->\nask pa [set pcolor patch-color]\n]
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
605
590
792
623
Recolor Patches by C-Beliefs
recolor-patches-by-c-beliefs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

INPUTBOX
1410
470
1562
530
Hawk-Color
4.0
1
0
Color

INPUTBOX
1410
590
1562
650
Dove-Color
9.0
1
0
Color

INPUTBOX
1410
530
1562
590
MSNE-Color
6.0
1
0
Color

INPUTBOX
1565
530
1717
590
MSNE-Margin
5.0
1
0
Number

PLOT
2010
10
2275
200
Actions
Ticks
% of People
0.0
1.0
0.0
100.0
true
true
"" ""
PENS
"MSNE" 1.0 0 -16777216 true "" ""

PLOT
1725
205
2005
395
Best Responses
Ticks
% of C-Beliefs
0.0
1.0
0.0
100.0
true
true
"" ""
PENS
"MSNE" 1.0 0 -16777216 true "" ""

BUTTON
285
85
350
118
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
285
120
350
153
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
280
155
350
188
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

INPUTBOX
1410
660
1562
720
Mixture-Color
65.0
1
0
Color

BUTTON
400
580
552
613
Highlight Always Hawk
foreach sorted-people [pp ->\nask pp [\nset size ifelse-value (\npp-always-hawk?\n) [2] [1]\n]\n]
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
400
615
552
648
Highlight Always Dove
foreach sorted-people [pp ->\nask pp [\nset size ifelse-value (\npp-always-dove?\n) [2] [1]\n]\n]
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
400
650
547
683
Highlight Play Mixture
foreach sorted-people [pp ->\nask pp [\nset size ifelse-value (\npp-play-mixture?\n) [2] [1]\n]\n]
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
400
685
477
718
Size = 1
foreach sorted-people [pp ->\nask pp [\nset size 1\n]\n]
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
15
370
187
403
Clustering-Weight
Clustering-Weight
0
10
2.0
1
1
NIL
HORIZONTAL

INPUTBOX
15
535
90
595
HD-Value
10.0
1
0
Number

MONITOR
95
545
212
590
HD Cost of Conflict
hd-cost-of-conflict
3
1
11

PLOT
805
10
1115
200
Most Frequent Interaction
Ticks
% of People
0.0
1.0
0.0
100.0
true
true
"" ""
PENS
"DD: Peace" 1.0 0 -13840069 true "" ""
"DH: Dominated" 1.0 0 -4079321 true "" ""
"HD: Dominant" 1.0 0 -14730904 true "" ""
"HH: Conflict" 1.0 0 -2674135 true "" ""

SLIDER
15
330
187
363
Inertia
Inertia
0
100
90.0
5
1
%
HORIZONTAL

CHOOSER
605
495
797
540
Color-People-By
Color-People-By
"Action" "Most Frequent Interaction" "Payoff" "Hawkishness"
1

BUTTON
605
455
717
488
Recolor People
recolor-people
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
245
430
355
475
NIL
Mouse-Patch
17
1
11

BUTTON
270
340
350
373
Highlight
highlight-current-person
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
260
380
355
425
NIL
Current-Person
17
1
11

BUTTON
270
305
350
338
Move
move-current-person
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
260
560
370
593
Memories
if is-person? current-person [\nask current-person [\nset label (word (map [f -> precision f 2] array:to-list pp-freqs) \"     \")\n]]
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
635
740
765
773
Shift People Up
foreach sorted-people [pp ->\nask pp [setxy xcor (ycor + 5)]\n]
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
635
775
765
808
Shift C-Beliefs Up
foreach sort c-beliefs [mn ->\nask mn [setxy xcor (ycor + 5)]\n]
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
260
540
400
558
Label Current-Person:
13
0.0
1

PLOT
2010
400
2320
590
Recent Payoff by MFI
Ticks
Payoff
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"DD: Peace" 1.0 0 -13840069 true "" ""
"DH: Dominated" 1.0 0 -4079321 true "" ""
"HD: Dominant" 1.0 0 -14730904 true "" ""
"HH: Conflict" 1.0 0 -2674135 true "" ""
"Noise" 1.0 0 -7500403 true "" ""
"Exp = MSNE" 1.0 0 -8630108 true "" ""

MONITOR
15
830
167
875
Expected Payoff at MSNE
msne-exp-payoff
3
1
11

MONITOR
15
720
72
765
D vs D
item 0 payoffs
3
1
11

MONITOR
75
720
132
765
D vs H
item 1 payoffs
3
1
11

MONITOR
15
770
72
815
H vs D
item 2 payoffs
3
1
11

MONITOR
75
770
132
815
H vs H
item 3 payoffs
3
1
11

TEXTBOX
15
695
165
713
Payoffs Table:
14
0.0
1

MONITOR
15
880
112
925
Payoff to Noise
noise-payoff
3
1
11

SLIDER
15
405
197
438
Perc-Right-Hand-Side
Perc-Right-Hand-Side
0
100
50.0
5
1
%
HORIZONTAL

SLIDER
845
555
1017
588
Statistics-Retention
Statistics-Retention
0
100
90.0
5
1
%
HORIZONTAL

SLIDER
1130
690
1337
723
MFI-Network-Radius
MFI-Network-Radius
0
12
3.9597979746446663
.25
1
Patches
HORIZONTAL

BUTTON
1130
600
1262
633
Show MFI Network
setup-mfi-network
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
1130
445
1317
490
Number of MFI Net Components
length mfi-component-sizes
17
1
11

INPUTBOX
845
605
1045
665
Ticks-Between-Network-Updates
100.0
1
0
Number

PLOT
1120
10
1380
200
MFI Network Components
Rank Order
Size
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"DD" 1.0 1 -13840069 true "" ""
"DH" 1.0 1 -4079321 true "" ""
"HD" 1.0 1 -14730904 true "" ""
"HH" 1.0 1 -2674135 true "" ""

PLOT
1120
205
1400
395
MFI Component Counts
Ticks
Count
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"All" 1.0 0 -16777216 true "" ""
"Size > 1" 1.0 0 -5825686 true "" ""
"Size > 5" 1.0 0 -2064490 true "" ""

MONITOR
1130
495
1257
540
MFI Components > 5
num-mfi-components-larger-than 5
17
1
11

MONITOR
1130
545
1337
590
Largest MFI Component [Size Type]
first mfi-component-sizes
17
1
11

SWITCH
1130
650
1272
683
Hide-MFI-Links?
Hide-MFI-Links?
1
1
-1000

BUTTON
1690
405
1877
438
Print InterType Hawkishness
print-inter-type-hawkishness
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
2010
205
2270
395
Self-Hawkishness by MFI
Ticks
% of Type
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"DD" 1.0 0 -13840069 true "" ""
"DH" 1.0 0 -4079321 true "" ""
"HD" 1.0 0 -14730904 true "" ""
"HH" 1.0 0 -2674135 true "" ""

INPUTBOX
15
975
167
1035
Seed-Setup
-1.925572583E9
1
0
Number

INPUTBOX
15
1040
167
1100
Seed-Go
4.62326508E8
1
0
Number

TEXTBOX
15
950
225
970
Random Number Generation:
14
0.0
1

BUTTON
1790
915
1902
948
Do Profile Test
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

BUTTON
170
995
232
1028
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
235
995
337
1028
Use Previous
set seed-setup previous-seed-setup
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
170
1060
232
1093
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

BUTTON
235
1060
337
1093
Use Previous
set seed-go previous-seed-go
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
190
350
223
Go 1000
repeat 1000 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
845
445
912
490
DD People
length item 0 sorted-people-by-mfi-type
17
1
11

MONITOR
915
445
982
490
DH People
length item 1 sorted-people-by-mfi-type
17
1
11

MONITOR
845
495
912
540
HD People
length item 2 sorted-people-by-mfi-type
17
1
11

MONITOR
915
495
982
540
HH People
length item 3 sorted-people-by-mfi-type
17
1
11

TEXTBOX
845
405
1010
445
Count People by their \nMost Frequent Interaction:
13
0.0
1

INPUTBOX
845
670
997
730
Ticks-Between-Plot-Updates
100.0
1
0
Number

PLOT
1725
10
1995
200
Beliefs Histogram
Degree of Belief
% of C-Beliefs
0.0
100.0
0.0
1.0
true
true
"" ""
PENS
"Data" 1.0 1 -14835848 true "" ""
"MSNE" 1.0 0 -16777216 true "" ""

TEXTBOX
1130
405
1345
445
Link People with Same Most Frequent Interaction:
13
0.0
1

MONITOR
1130
730
1287
775
Expected People Per Circle
people-per-mfi-circle
3
1
11

BUTTON
1130
780
1247
813
Re-Scale Radius
rescale-mfi-network-radius
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
1410
740
1562
800
Patch-Color
9.9
1
0
Color

BUTTON
15
1105
147
1138
NIL
setup-demo-seeds
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
1425
250
1522
295
Peace: D vs D
perc-events-dd
1
1
11

MONITOR
1590
250
1687
295
Conflict: H vs H
perc-events-hh
1
1
11

MONITOR
1475
300
1642
345
Dominance: H vs D or D vs H
perc-events-dhhd
1
1
11

TEXTBOX
1425
210
1585
240
Count Interactions by Type (% of Total):
13
0.0
1

PLOT
1405
10
1720
200
Interaction Events
Ticks
% of Total
0.0
1.0
0.0
100.0
true
true
"" ""
PENS
"DD: Peace" 1.0 0 -13840069 true "" ""
"Dominance" 1.0 0 -16777216 true "" ""
"HH: Conflict" 1.0 0 -2674135 true "" ""

CHOOSER
15
245
167
290
Initial-C-Belief-Positions
Initial-C-Belief-Positions
"Random" "Ring Around (0, 0)" "Ring Around Agent" "At Agent" "At Other Agents"
0

SLIDER
15
450
187
483
Epsilon
Epsilon
0
10
0.0
0.5
1
%
HORIZONTAL

TEXTBOX
850
825
1000
845
Simulating Shocks:
16
0.0
1

INPUTBOX
850
850
1002
910
Ticks-Until-Shock
-1.0
1
0
Number

SWITCH
850
915
992
948
Shock-Repeats?
Shock-Repeats?
0
1
-1000

CHOOSER
850
955
1172
1000
Shock-Code
Shock-Code
"" "set Base-MSNE 100 - Base-MSNE" "set Inertia 100 - Inertia" "set Memory 100 - Memory" "set Memory 100 - Memory set Inertia 100 - Inertia" "set Epsilon ifelse-value (Epsilon = 0) [5] [0]"
0

MONITOR
850
1010
942
1055
NIL
Time-Of-Shock
17
1
11

TEXTBOX
1010
855
1130
896
To prevent any shocks, set Ticks-Until-Shock to a value < 0.\n
11
0.0
1

TEXTBOX
1000
915
1150
941
Shock repeats every Ticks-Until-Shock.
11
0.0
1

SWITCH
400
905
540
938
Hide-C-Beliefs?
Hide-C-Beliefs?
0
1
-1000

SWITCH
400
870
540
903
Hide-People?
Hide-People?
1
1
-1000

TEXTBOX
400
840
585
871
Initially Hide Objects and Links?
13
0.0
1

CHOOSER
195
245
352
290
Initial-Person-Attributes
Initial-Person-Attributes
"Random" "4 Clusters" "Square Grid" "Triangular Grid" "Random-Patch" "Gaussian"
0

PLOT
805
205
1115
395
Inter-C-Belief-Distance by MFI
Ticks
% of World Max
0.0
1.0
0.0
50.0
true
true
"" ""
PENS
"DD: Peace" 1.0 0 -13840069 true "" ""
"DH: Dominated" 1.0 0 -4079321 true "" ""
"HD: Dominant" 1.0 0 -14730904 true "" ""
"HH: Conflict" 1.0 0 -2674135 true "" ""

PLOT
2010
640
2210
790
Histogram
Value
Count
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

CHOOSER
2010
795
2282
840
Histogram-Values
Histogram-Values
"[ticks - cb-date-of-activation] of c-beliefs" "[cb-degree] of c-beliefs" "[pp-hawkishness] of people"
1

BUTTON
2010
845
2137
878
Update Histogram
do-histogram
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
400
730
587
775
Color-C-Beliefs-By
Color-C-Beliefs-By
"Degree" "Time Between Activations"
0

BUTTON
400
780
522
813
Recolor C-Beliefs
recolor-c-beliefs
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
605
625
810
658
Recolor Patches by CB Distance (I)
recolor-patches-by-distance-to-c-beliefs true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
15
195
217
240
Initial-Degree-Of-Belief
Initial-Degree-Of-Belief
"Base-MSNE" "Random-Float 100" "0" "50" "100" "100 - Base-MSNE" "Bernoulli 50" "Bernoulli Base-MSNE" "Bernoulli (100 - Base-MSNE)"
0

PLOT
850
1120
1050
1270
Stochastic Action
Belief(Opp H)
Pr(My H)
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

BUTTON
1055
1120
1127
1153
Update
set-current-plot \"Stochastic Action\"\nclear-plot\nforeach n-values 21 [n -> 5 * n] [d -> plotxy d (100 - renorm-belief d)]
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
1055
1160
1207
1220
Slope-Multiplier
20.0
1
0
Number

SLIDER
1055
1225
1227
1258
Base-MSNE
Base-MSNE
0
100
90.0
5
1
%
HORIZONTAL

CHOOSER
15
610
177
655
Response-Choice
Response-Choice
"Best" "Stochastic" "MSNE"
0

INPUTBOX
205
700
357
760
Halting-Sample-Size
200.0
1
0
Number

SLIDER
205
765
382
798
Halting-Tests-Required
Halting-Tests-Required
0
20
20.0
1
1
NIL
HORIZONTAL

SWITCH
205
810
342
843
Halt-On-Tests?
Halt-On-Tests?
1
1
-1000

INPUTBOX
205
865
357
925
Halting-Slope-Bound
0.02
1
0
Number

MONITOR
225
480
382
525
NIL
Num-Halting-Tests-Passed
17
1
11

INPUTBOX
1530
900
1645
960
HH-Color
15.0
1
0
Color

INPUTBOX
1410
900
1525
960
HD-Color
103.0
1
0
Color

INPUTBOX
1530
835
1645
895
DH-Color
44.0
1
0
Color

INPUTBOX
1410
835
1525
895
DD-Color
65.0
1
0
Color

BUTTON
605
660
810
693
Recolor Patches by CB-Distance (II)
recolor-patches-by-distance-to-c-beliefs false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
# Play Best Response Given Context-Dependent Beliefs
## WHAT IS IT?

Simulates interacting game players ("people") whose moves are the best responses to their expectations concerning their opponent's next move. Expectations are based on any past experiences of playing opponents of the samee type. Each player has a number of models ("C-Beliefs") which they use first to identify an opponent's type, and then to decide on the best action to perform against an opponent of that type.

The game chosen here is __Hawk-Dove__, or Chicken. Hawk-Dove, in common with __Mini-Nash Demand__, has a __mixed strategy Nash Equilibrium (MSNE)__. If a player's belief that their opponent will play Hawk is equal to MSNE, then that player is indifferent between playing "Hawk" and playing "Dove". A player of such games has the following decision rules:

* If Belief < MSNE, then play "Hawk"
* If Belief > MSNE, then play "Dove"
* If Belief = MSNE, then play a neutral strategy. (We use: Play "Hawk" with probability = MSNE, otherwise play "Dove".)
 

This program (C) Christopher J Watts, 2024.

This model accompanies the paper:

Watts, Christopher J. & S. M. Amadae (2024) "Context-sensitive game players: How context-specific expectations affect the emergence of group conflict, peace, and dominance in Hawk-Dove and Nash Demand games". Submitted to the Social Simulation Conference 2024, Krakow, Poland.

## HOW IT WORKS

A population of people is created. Each person has two attributes, represented here by their xcor and ycor. Each person has a number of models (C-Beliefs). Each C-Belief also relates to a position in 2D space. The position of the C-Belief determines its relevance to interactions with a given opponent. Each C-Belief stores a belief about relevant opponents. This belief is initialialized as the MSNE.
 
Each time step, players are paired up randomly. Each player identifies their most relevant C-Belief, given the attributes of their current opponent. The most relevant node is the C-Belief closest to the opponent in 2D space, with ties settled randomly. Each player plays their best response to the opponent, given their belief described in their C-Belief. (With probability _Epsilon_ they may switch to explore the opposite action.) On experiencing their opponent's action, each player updates the belief stored in their relevant C-Belief.

## HOW TO USE IT

Choose an MSNE. Choose the number of people, and the number of C-Beliefs per person.

Click "Setup".

Click a "Go" button.

View the time series plots to see:

* what % of people experience each type of interaction as their Most Frequent Interaction (MFI): D-vs-D, DH, HD, or HH
* average payoff being received by people classified by their Most Frequent Interaction (MFI): DD, DH, HD, or HH
* what % of C-Beliefs support the playing each best response action, given the beliefs
* what % of interaction events involved Conflict (H-vs-H), Peace (DD), or Dominance (HD / DH)
* what % of people have just played "Hawk", or just playeed "Dove"
* how many network components are there, if we link people with the same MFI and within a given radius of each other within attribute space
* what % of the people in each MFI type would play "Hawk" against an opponent with the same attributes as themselves ("Self-Hawkishness")

People, C-Beliefs, and people's links to their C-Beliefs (C-Links) can be hidden or unhidden. Patches can be shaded to reflect the most popular of neighboring people's actions, and to reflect the most popular of neighboring C-Beliefs' implied best responses, given their beliefs.

## THINGS TO NOTICE

With sufficient numbers of people, there emerge visually identifiable areas of the world in which a particular action predominates among the players, or a particular best response predominates among the C-Beliefs. Use Patch Shading to make this more visible.

## THINGS TO TRY

What varies with the number of C-Beliefs per person?

What varies with the discounting parameters, _Memory_ and _Inertia_?

How robust are outcomes if agents engage in a small amount of exploration (_Epsilon_ > 0), rather than playing their best response automatically?

What if a player's C-Beliefs were moved, perhaps to better discriminate between recent Hawk-players and Dove-players?

## EXTENDING THE MODEL

What if instead of working with most relevant (i.e. nearest) C-Belief each time, agents draw upon all their C-Beliefs, weighted by relevance, to choose actions, and update all their C-Beliefs, again in proportion to relevance? E.g. by using the mechanisms of Artificial Neutal Networks.

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

This model builds upon previous studies of the Red King and Red Queen Effects (Amadae & Watts 2022; Bruner 2019; O'Connor 2017; Bergstrom & Lachmann 2003), and of game-playing agents who perform belief learning (Axtell et al. 2001).

Note that group-like patterns emerge among the agents without anyone imitating others directly (in contrast with Axelrod 1997) or being subject to social influence (in contrast with Hegselmann & Krause 2002).

## CREDITS AND REFERENCES

Amadae, S. M., & Watts, C. J. (2022). Red Queen and Red King Effects in cultural agent-based modeling: Hawk Dove Binary and Systemic Discrimination. The Journal of Mathematical Sociology, 1-28. https://doi.org/10.1080/0022250X.2021.2012668 

Axelrod, R. M. (1997). The dissemination of culture - A model with local convergence and global polarization [Article]. Journal of Conflict Resolution, 41(2), 203-226. https://doi.org/10.1177/0022002797041002001 

Axtell, R., Epstein, J. M., & Young, H. P. (2001). The emergence of classes in a multiagent bargaining model. In S. N. Durlauf & H. P. Young (Eds.), Social Dynamics - Economic Learning and Social Evolution (pp. 191-211). MIT Press. 

Bergstrom, C. T., & Lachmann, M. (2003). The Red King effect: When the slowest runner wins the coevolutionary race. Proceedings of the National Academy of Sciences, 100(2), 593-598. https://doi.org/10.1073/pnas.0134966100 

Bruner, J. P. (2019). Minority (dis)advantage in population games. Synthese, 196(1), 413-427. https://doi.org/10.1007/s11229-017-1487-8 

Hegselmann, R., & Krause, U. (2002). Opinion dynamics and bounded confidence: models, analysis and simulation [Article]. Jasss-the Journal of Artificial Societies and Social Simulation, 5(3). 

O’Connor, C. (2017). The cultural Red King effect. The Journal of Mathematical Sociology, 41(3), 155-171. https://doi.org/10.1080/0022250X.2017.1335723 
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
<experiments>
  <experiment name="experiment_Base" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefs" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="16"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefsMatched" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="64"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;At Other Agents&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_MSNE" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Inertia" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="99"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Memory" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
      <value value="52.5"/>
      <value value="55"/>
      <value value="57.5"/>
      <value value="60"/>
      <value value="62.5"/>
      <value value="65"/>
      <value value="67.5"/>
      <value value="70"/>
      <value value="72.5"/>
      <value value="75"/>
      <value value="77.5"/>
      <value value="80"/>
      <value value="82.5"/>
      <value value="85"/>
      <value value="87.5"/>
      <value value="90"/>
      <value value="92.5"/>
      <value value="95"/>
      <value value="97.5"/>
      <value value="99"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="20000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Pop" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="16"/>
      <value value="23"/>
      <value value="32"/>
      <value value="45"/>
      <value value="64"/>
      <value value="91"/>
      <value value="128"/>
      <value value="181"/>
      <value value="200"/>
      <value value="256"/>
      <value value="362"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_InitAttribs" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="16"/>
      <value value="36"/>
      <value value="64"/>
      <value value="100"/>
      <value value="144"/>
      <value value="196"/>
      <value value="256"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
      <value value="&quot;4 Clusters&quot;"/>
      <value value="&quot;Square Grid&quot;"/>
      <value value="&quot;Triangular Grid&quot;"/>
      <value value="&quot;Random-Patch&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefs_Pop" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <exitCondition>Number-Of-C-Beliefs &gt; Number-Of-People</exitCondition>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="16"/>
      <value value="36"/>
      <value value="64"/>
      <value value="100"/>
      <value value="144"/>
      <value value="196"/>
      <value value="256"/>
      <value value="324"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
      <value value="&quot;At Other Agents&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
      <value value="&quot;Triangular Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="4000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Inertia_v_Memory" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="92.5"/>
      <value value="95"/>
      <value value="97.5"/>
      <value value="99"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Inertia_v_CBeliefs" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="100"/>
      <value value="140"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="92.5"/>
      <value value="95"/>
      <value value="97.5"/>
      <value value="99"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Inertia_v_MSNE" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="92.5"/>
      <value value="95"/>
      <value value="97.5"/>
      <value value="99"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefsMatched_MSNE" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="64"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;At Other Agents&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefsMatched_Memory_MSNE_Ine" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="64"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;At Other Agents&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefsMatched_Inertia_MSNE_Mem" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="64"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;At Other Agents&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefs_Inertia_MSNE_Mem" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="48"/>
      <value value="64"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="20"/>
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_MSNE_Variations" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="100"/>
      <value value="90"/>
      <value value="70"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="50"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Exp1_MSNE" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Exp2_StatRet" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Exp3_CBeliefs" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="0"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="48"/>
      <value value="64"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Exp4_Inertia" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="48"/>
      <value value="64"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Exp5_Memory" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="0"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Exp6_Inertia_And_Memory" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="120"/>
      <value value="160"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="0"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Exp7_Pos" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
      <value value="120"/>
      <value value="160"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="0"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;At Other Agents&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Mem50_MSNE_InitDeg" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
      <value value="&quot;0&quot;"/>
      <value value="&quot;50&quot;"/>
      <value value="&quot;100&quot;"/>
      <value value="&quot;Random-Float 100&quot;"/>
      <value value="&quot;100 - Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Mem50_MSNE_ResponseChoice" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Response-Choice">
      <value value="&quot;Stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Slope-Multiplier">
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_MSNE_Memory" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="100"/>
      <value value="90"/>
      <value value="70"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="50"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_MSNE_Inertia" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="50"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Memory_CBeliefs" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="62.5"/>
      <value value="65"/>
      <value value="67.5"/>
      <value value="70"/>
      <value value="72.5"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="97.5"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Memory_Inertia_CBeliefs_MSNE" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="50"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="62.5"/>
      <value value="65"/>
      <value value="67.5"/>
      <value value="70"/>
      <value value="72.5"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="97.5"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="100"/>
      <value value="95"/>
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Inertia_Memory_CBeliefs_MSNE" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="92.5"/>
      <value value="95"/>
      <value value="97.5"/>
      <value value="99"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefsMatched_MSNE_Ine_Mem" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="64"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;At Other Agents&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Pop_CBeliefs_MSNE_Ine_Mem" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="16"/>
      <value value="23"/>
      <value value="32"/>
      <value value="45"/>
      <value value="64"/>
      <value value="91"/>
      <value value="128"/>
      <value value="181"/>
      <value value="200"/>
      <value value="256"/>
      <value value="362"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefs_Pop_Ine_Mem_MSNE" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_MSNE75_Mem" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Memory" first="50" step="2" last="90"/>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Mem50_MSNE75" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <steppedValueSet variable="Base-MSNE" first="70" step="1" last="80"/>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Memory" first="45" step="1" last="55"/>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Inertia_Memory_CBe" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Memory" first="70" step="2" last="100"/>
    <steppedValueSet variable="Inertia" first="70" step="2" last="100"/>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_CBeliefs_IP_Inertia" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="16"/>
      <value value="24"/>
      <value value="32"/>
      <value value="48"/>
      <value value="64"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="0"/>
      <value value="20"/>
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;At Other Agents&quot;"/>
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="20000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Pop_CBe" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>do-final-stats</final>
    <metric>previous-seed-setup</metric>
    <metric>previous-seed-go</metric>
    <metric>timer</metric>
    <metric>hd-cost-of-conflict</metric>
    <metric>msne-exp-payoff</metric>
    <metric>noise-payoff</metric>
    <metric>item 0 payoffs</metric>
    <metric>item 1 payoffs</metric>
    <metric>item 2 payoffs</metric>
    <metric>item 3 payoffs</metric>
    <metric>count people</metric>
    <metric>count C-Beliefs</metric>
    <metric>perc-played-hawk</metric>
    <metric>perc-played-dove</metric>
    <metric>perc-interaction-type 0</metric>
    <metric>perc-interaction-type 1</metric>
    <metric>perc-interaction-type 2</metric>
    <metric>perc-interaction-type 3</metric>
    <metric>mean-payoff-by-mfi 0</metric>
    <metric>mean-payoff-by-mfi 1</metric>
    <metric>mean-payoff-by-mfi 2</metric>
    <metric>mean-payoff-by-mfi 3</metric>
    <metric>perc-events-dd</metric>
    <metric>perc-events-hh</metric>
    <metric>perc-events-dhhd</metric>
    <metric>perc-always-hawk</metric>
    <metric>perc-play-mixture</metric>
    <metric>perc-always-dove</metric>
    <metric>perc-best-response-dove</metric>
    <metric>perc-best-response-msne</metric>
    <metric>perc-best-response-hawk</metric>
    <metric>perc-beliefs-between 0 20</metric>
    <metric>perc-beliefs-between 20 40</metric>
    <metric>perc-beliefs-between 40 60</metric>
    <metric>perc-beliefs-between 60 80</metric>
    <metric>perc-beliefs-between 80 101</metric>
    <metric>num-mfi-components-larger-than 0</metric>
    <metric>num-mfi-components-larger-than 1</metric>
    <metric>num-mfi-components-larger-than 5</metric>
    <metric>first first mfi-component-sizes</metric>
    <metric>last first mfi-component-sizes</metric>
    <metric>perc-self-hawkish-mfi-type 0</metric>
    <metric>perc-self-hawkish-mfi-type 1</metric>
    <metric>perc-self-hawkish-mfi-type 2</metric>
    <metric>perc-self-hawkish-mfi-type 3</metric>
    <metric>mean-icb-distance-by-mfi 0</metric>
    <metric>mean-icb-distance-by-mfi 1</metric>
    <metric>mean-icb-distance-by-mfi 2</metric>
    <metric>mean-icb-distance-by-mfi 3</metric>
    <enumeratedValueSet variable="Base-MSNE">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-C-Beliefs">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Inertia">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Epsilon">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-People">
      <value value="16"/>
      <value value="23"/>
      <value value="32"/>
      <value value="45"/>
      <value value="64"/>
      <value value="91"/>
      <value value="128"/>
      <value value="181"/>
      <value value="200"/>
      <value value="256"/>
      <value value="362"/>
      <value value="512"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Degree-Of-Belief">
      <value value="&quot;Base-MSNE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-C-Belief-Positions">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Attributes">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Perc-Right-Hand-Side">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Clustering-Weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Statistics-Retention">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Length">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MSNE-Margin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HD-Value">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MFI-Network-Radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Network-Updates">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Between-Plot-Updates">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ticks-Until-Shock">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Repeats?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Shock-Code">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Setup">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed-Go">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
