------------------------
title: Proving MU Viability
page-order: 2
blurb: >
  Descriptions of important data and parameters that we can use to gauge whether or not the MU would work in a given society, and how to calculate them from existing data
------------------------

## Definitions of Socioeconomic Parameters

Before the MU can be implemented, it first needs to be proven that its fundamental assumption is true: human society in 2021 has a high enough technology level to support universal equal rights as stipulated by the [MSC][]. The evidence for this must be convincing and easily verifiable for the widest possible audience. To start with, we will need to define terms and parameters that are used for economic management in the MU and by which we measure success.

These parameters are based around the concept of a *standard person*, which is an abstract reference point which should never be taken to imply participation or ability required of any real person. Rather, we assume the existence of a hypothetical society where each person has free access to basic goods and services and chooses to work as much as they want in order to afford to purchase any nonessential goods and services they choose on the open market. Some people may not be willing or able to work at all, and are content to live comfortable but minimalistic lives on basic services; some may occasionally work in nonessential jobs in order to afford moderate amenities or leisure activities; and some may be willing to devote a significant amount of their time in the service of society as a whole, and be rewarded with enough private income to live in relative but not excessive luxury. Out of all the individuals in this hypothetical society, we define a standard person as one with unremarkable upbringing, median ability, and median working hours.

- *Baseline Workload* ($w_b$) is the minimum number of hours per week a standard person would need to work in order to guarantee the MSC equally across the entire population. A fundamental assumption which needs to be proven (and is explored in more detail below) is that in today’s high-tech human societies, this number is much, much less than 40 hours per week, which is the standard number an adult in the US is expected to meet in 2021 to live a "normal" life. This value should be expected to fluctuate over large enough periods of time, but should be more or less stable within a given 10-year period, and should never have huge short-term swings except in response to catastrophic events affecting the behavior of every person in the society.
- *Standard Workload* ($w_s$) is the number of hours per week a standard person would need to work in order to live in moderate comfort (better than baseline but not extravagant). It defines the peak of the bell curve describing how many people in any society work a given number of hours per week.
- *Standard Productive Lifetime* ($L$) is the number of weeks a standard person is expected to be of working age, between legal adulthood and retirement. Multiplying this by a workload $w$ yields the number of working hours $W$ in a standard person's lifetime. Note that "retirement" does not imply a particular age; it is the point at which a person chooses to stop working, and continues to live off of savings and basic services for the rest of their life. This choice is entirely the prerogative of each individual person; the standard productive lifetime is merely the median such value across all individuals in a given society.
- *Social Affluence* ($A$) is a dimensionless metric which tracks how much more citizens work than they strictly need to in order to keep society functioning. It is defined as $A=\log(W_s / W_b)$. If this is negative, MU ceases to be able to guarantee the MSC because not enough people are willing to work enough to maintain basic social services. This value can fluctuate on short time scales in response to significant technological advancements or emergencies, but in the absence of either, should remain fairly stable after gradually rising to a limiting value that represents the highest sustainable level of leisure activity in a given environment.

## Important Assumptions & Data Requirements

A society only works if enough people believe, without being coerced, that it does and will continue to do so throughout their lifetime. Human history is rife with examples of revolutions that failed, or descended into chaos/terror/violence, because the leaders allowed personal rights to be abridged in the interest of implementing their own view of a "correct" society.

At this point in history, we have access to something those failed revolutionaries didn’t have: an abundance of data with which to prove that the MU could work (or at least determine what factors make it impossible, which would be useful but disappointing) and adequately plan the management of its critical economy and reasonable regulation of an open market for the noncritical economy. It is impossible to overemphasize that people must be able to believe in the workability of this society without needing to be coerced. Any emerging pattern of coercion (physical or emotional) to achieve social goals must be prevented from taking root at all costs, including abandoning the MU altogether if that is the only way to prevent the development of systemic coercion or corruption. "Coerced utopia" is just another name for fascism.

In the interest of avoiding the kinds of traps that can lead to the development of a coercive state, anyone who works for the MU government must agree to uphold and defend the following Social Openness Principle: any data that is used to allocate resources or calculate any of the social/economic parameters defined above must be publicly available and subject to peer review by the widest possible audience, regardless of MU citizenship. It should be easy for any educated adult to get nontechnical explanations of the data sources and fundamental assumptions and reasoning; as well as for experts to examine and contribute to the technical data.

### Questions to Answer for MU Viability

- Housing (urban/suburban/rural environment)
    - How many people could be safely housed right now, given existing infrastructure?
    - How much does that number change over time?
    - How many person-hours of effort does it take to house one standard person with basic utilities?
    - How many people could conceivably be sustainably housed on Earth?
- Ecology
    - How many person-hours of effort will it take to restore a balanced ecosystem on Earth?
    - How many person-hours per year will be needed to maintain that balanced ecosystem?
- Public Safety
    - How many person-hours of effort does it take to reasonably ensure public defense and timely and effective emergency response?
- Food
    - How many calories are produced on average each day?
    - What if agriculture was limited to sustainable and non-exploitative means? 
    - How many person-hours of effort are required to sustain the food needs of a standard person?
- Healthcare
    - How many person-hours of medical care (including training for nurses/doctors) are required for a standard person over their lifetime?
- Luxuries/Leisure
    - How much does a standard person spend on nonessential goods or services?

Add up all the person-hours per standard person per week for each of the essentials above; this is $w_b$. MU is not viable unless $w_b \ll 40$, because no one is willing to work 40 hours a week just to ensure basic services.

A first approximation for the standard workload ws can be calculated by computing an equivalent "social affluence" for today’s consumer-capitalist society. $A_{cap}=\log(E_{ne}/E_e)$, where $E_{ne}$ and $E_e$ are median expenditures on non-essential and essential goods/services, respectively. Then $w_s = w_b \times E_{ne} / E_e$. This relationship should not be interpreted as a "law", but merely a convenient rule of thumb to maintain approximately the same standard of living for an average person when transitioning from consumer capitalism.  
