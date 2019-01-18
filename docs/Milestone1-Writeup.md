**DSCI-532: Milestone 1**

**Write-up**

*Design Rationale*

We designed our visualization app with the intention of conveying how both individual demographics and workplace dynamics relate to mental health attitudes and experiences. With this goal in mind, we first focused on how we wanted to provide the user with what we considered the most interesting data related to this intent. On the left-hand panel our design contains several sets of categorical buttons (ex. gender, anonymity, treatment), and sliders (ex. age, work freedom) that we believe offer flexibility in understanding how questions around workplace mental health may differ based on individual characteristics and geography. These questions of interest are displayed on the left-hand panel at the top, and considered the most intriguing and potentially useful to users. In order to avoid an overwhelming set of panel options, we chose to represent geographical differences using a heat map of the United States (where the majority of our subjects live) for displaying state-by-state differences to our questions, and provide a bar chart comparison between countries for international differences as well. Both cultural and individual questions are important to highlight in this dataset, and we tried to inform our design with this balance kept in mind.

*Data Abstraction*

Almost all of the variables in this dataset are ordinal-based text, meaning we created numerical scores out of 'yes','no','don't know' responses for the majority of columns in order to represent the information numerically in our visual displays.

Our 'work freedom' characteristic (left-hand panel) is an aggregate measure  we created from multiple variables (self-employed, wellness program offers, benefits offers, remote work offers, and ability to take leave of absence) that relate to employee perception of their 'freedom' at work. The work freedom score was calculated based on answers to these 5 questions (yes/no/don't know) with a score between -5 and 5. We chose to implement a slider since it provides the ability for the user to select their range of interest for the freedom variable.



*Reflection on Proposal & Initial App*

One issue we didn't foresee in our proposal stage was the lack of observations for many countries in our dataset, and the problems this would present in depicting meaningful cultural distinctions around mental illness. It's difficult to draw conclusions about cultural differences when there are so few observations for many european and asian countries compared to the United States, therefore we altered our goal to convey differences between U.S. states more clearly with our heat map, and only consider countries with a reasonable number of observations (Canada, US, UK, among a few others) to present a fair comparison.

Generally speaking, I believe we address a range of interesting questions around stigma, open communication, mental health awareness, and treatment seeking behaviors quite well. These are important questions that are complex, however, they can be addressed much more simply when the user has more control over a range of characteristics tied to these questions. This was the central focus for us in developing our idea, and believe we've empowered the user to explore all sorts of concerns about employee mental health in the workplace.

### USAGE QUESTION BELOW
With respect to our usage, I know a concern in our proposal's evaluation was that the data wasn't personalized to someone working within a given tech company, however, these are general trends that exist for a reason. They display relationships between demographic and geographic criteria and mental illness, which is quite useful to companies surveying their own internal procedures and employees. How old are our employees and what age range reflects greatest prevalence in tech companies? Does this change based on branches in different countries? Are we providing enough education to our employees around the benefits we provide, and wellness programs made available? These are all central questions that our app depicts trends for, so we would hope this could be used as a reference for those inside companies looking to raise awareness about workplace dynamics related to mental health and illness.



Your writeup should include the rationale for your design choices, focusing on the interaction aspects and connecting these choices to a data abstraction (including a characterization of the raw data types and their scale/cardinality, and of any derived data that you decide compute) and the task abstraction (including a more detailed breakdown of tasks that arise from the goal stated above). You should also concisely describe your visual encoding choices.

Talk about how your vision has changed since your proposal

How have your visualization goals changed?
Does your app enable the tasks you set out to facilitate?