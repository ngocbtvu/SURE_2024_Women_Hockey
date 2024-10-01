# Evaluating Individual Defensive Contributions on the Penalty Kill

## Overview
This project was conducted during the **CMU Sports Analytics Camp**. We explored the dynamics of passes during power plays and how defensive actions influence the success rate of offensive plays using data from the **Big Data Cup** competitions. The goal is to quantify defensive impact during these events. 

- [Report](https://www.stat.cmu.edu/cmsac/sure/2024/showcase/hockey_sabres/report.html)
- [Poster](https://www.stat.cmu.edu/cmsac/sure/2024/showcase/hockey_sabres/poster.pdf)
- [Presentation](https://www.stat.cmu.edu/cmsac/sure/2024/showcase/hockey_sabres/slides.pdf)

## Introduction
In ice hockey, a **penalty kill** occurs when a team is down one or more players due to a penalty, giving the opposing team a numerical advantage, known as a **power play**. Effective defense during a penalty kill is crucial as the offense is more likely to score. Our project focuses on analyzing defensive actions that disrupt passes during power plays, as stalling the offense is key to a successful penalty kill.

## Data
The analysis uses data from the **Big Data Cup** competitions, which includes:
- Play-by-play data from 29 women’s hockey games (including the 2018 and 2022 Winter Olympics and the 2019 & 2024 Rivalry Series).
- Player-tracking data from the 2022 Winter Olympics, cleaned by Alon Harell, Robyn Ritchie, and Phil Shreaves.

## Methods
### Clustering Analysis
To identify common pass types, we applied **K-Means clustering** to categorize passes based on their start and end locations. We identified 10 pass clusters, such as **D-to-D** (Defenseman to Defenseman) and **Cross-Ice** (Passes across the offensive zone) pass.

### Pass-Completion Model
We developed an **XGBoost binary classification model** to predict the likelihood of pass completion. The model considered features such as distance from the closest defender to the passing lane and number of defenders crowding the passing lane.

The model was trained using **cross-validation** to ensure reliable predictions.

## Key Findings
- **Cross-Ice passes** were the most impactful in both offensive success and defensive disruption.
- **SHAP analysis** highlighted the importance of defender positioning and pass distance in determining pass success.

## Discussion
### Limitations
- Limited player-tracking data restricted the analysis to a subset of power plays.
- Inaccurate player identification and out-of-bounds coordinates in the original tracking data.

### Future Work
Future improvements could include:
- Accounting for all defenders on the ice, rather than just the nearest one.
- Incorporating defensive player speed and momentum into the model.
- Evaluating pass threat using **Expected Goals (xG)** and **Expected Possession Added Value (xPAV)**.

## Acknowledgements
We extend our thanks to:
- **Sam Ventura** for his guidance and expertise.
- **Ron Yurko**, **Quang Nguyen**, and our TAs for their support during the project.

## Citations
- [Big Data Cup 2022 Submission by Alon Harell, Robyn Ritchie, and Phil Shreaves](https://github.com/picagrad/Big-Data-Cup-2022)

## Author Information
- Christina Vu, Texas Christian University, ngoc.bt.vu@gmail.com
- Frithjof Sanger, Carnegie Mellon University, fsanger@andrew.cmu.edu
- Ian A. Peréz, University of Arizona, ianaperez@arizona.edu



    
