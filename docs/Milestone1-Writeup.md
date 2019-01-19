**DSCI-532: Milestone 1**

**Write-up**

*Design Rationale*

Our rationale when designing this visualization app was to convey how both individual demographics, geography and workplace dynamics relate to mental health attitudes and experiences. With this goal in mind, we first focused on how we wanted to provide the user with what we considered the most interesting data at their finger tips and convey it in plots that complimented one another. Below you will find a brief outline of what we implemented and the reasoning behind this, and where it follows our plan we laid initially out in our proposal as well as where we decided to go in a different design direction.

1. **Left-Hand Panel**: As discussed in our proposal, we included demographic and workplace flexibility information that we represented with both radiobuttons and a slider. One minor change is we removed 'anonymity' because we felt there were more than enough options for the user to play with already regarding age, gender, work flexibility, family history, and whether someone sought treatment. Our concern was that we did not want to overcrowd the panel and overwhelm the user. We chose categorical and continous widgets because the information we wanted to represent was both continuous (ex. age) and categorical (ex. family history of mental illness yes/no/don't know), which followed the ideas we laid out in our proposal. With respect to our questions of interest, we followed through on our proposal to offer a drop-down selection menu for various mental health questions someone would like to explore in relation to the other widgets discussed.

2. **Map**: Originally, we had intended to implement an interactive world map where someone could click on a country and receive a host of information related to mental health within the country. Unfortunately, we soon realized there were significant data constraints in our dataset that prevented us from conveying reasonable amounts of information for the majority of countries. In fact, most of our data comes from the U.S. and U.K., so our world map would be much less useful. In the end, we implemented a U.S. map to convey negative/positive scores for each U.S. state relating to mental health, and we think this captures some interesting complexities quite nicely.

3. **Graphs**: Our intention in the proposal was to implement a pie graph in addition to a bar graph where the bar chart would capture comparisons between countries, and the pie chart would capture information within an individual country showing proportions for each response (yes/no/don't know). When the construction phase of our app began we concluded that it would be quite redundent since the information could also be conveyed with a group bar chart for each of the 3 responses per country. The responses were normalized so control for a larger proportion of U.S. observations than other countries like Canada that we still wanted to compare. In addition, we felt it unnecessarily occupied alot of space on our dashboard and in the end decided to remove it. 

*Vision & Next Steps*

The first feature that would be useful to implement is we would like our graph to be interactive, where if you hover over a particular state it conveys useful information that our charts may or may not express. In addition, given how much of our information is customizable, it would be nice if we can find a way to implement changes to our graphs and maps more smoothly to the user. In addition, we may want to consider tabs the user can click on if there is other information we feel is left out that is important to show. Many of our next steps we feel will be cleaning up the appeal, and making it look much visually appealing as well.

*Usage*

In considering our proposal's usage we've decided that although this information may still be useful to companies who want to understand workplace dynamics and mental health, it may best be suited to clinical researchers interested in general geographical, demographic relationships with respect to mental health and the workplace. Given the constraints of the data, it's much harder for a company to inform it's procedures on information that doens't come from their own employees, in comparison to scientists who may be interested in general mental health trends. In general though, we feel our app does a great job providing data to the user about a host of questions, while giving them the flexibility to shape the answers they receive to those questions.


