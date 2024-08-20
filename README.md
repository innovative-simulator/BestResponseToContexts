# BestResponseToContexts
Agents playing the Hawk-Dove game choose the best response given their expectations, but they maintain different expectations for different contexts. As a result, group-like patterns of behaviour emerge.

This model accompanies the paper:

Watts, Christopher J. & S. M. Amadae (2024) "Context-sensitive game players: How context-specific expectations affect the emergence of group conflict, peace, and dominance in Hawk-Dove and Nash Demand games". Submitted to the Social Simulation Conference 2024, Krakow, Poland.

# Play Best Response Given Context-Dependent Beliefs
## WHAT IS IT?

Simulates interacting game players ("people") whose moves are the best responses to their expectations concerning their opponent's next move. Expectations are based on any past experiences of playing opponents of the samee type. Each player has a number of models ("C-Beliefs") which they use first to identify an opponent's type, and then to decide on the best action to perform against an opponent of that type.

The game chosen here is __Hawk-Dove__, or Chicken. Hawk-Dove, in common with __Mini-Nash Demand__, has a __mixed strategy Nash Equilibrium (MSNE)__. If a player's belief that their opponent will play Hawk is equal to MSNE, then that player is indifferent between playing "Hawk" and playing "Dove". A player of such games has the following decision rules:

* If Belief < MSNE, then play "Hawk"
* If Belief > MSNE, then play "Dove"
* If Belief = MSNE, then play a neutral strategy. (We use: Play "Hawk" with probability = MSNE, otherwise play "Dove".)
 

This program (C) Christopher J Watts, 2024.

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
