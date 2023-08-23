# 2024-Big-Data-Bowl
Repo for 2024 Big Data Bowl Submission

# Big Data Bowl steps
## Setup
* Set Up github repo for working on the files concurrently
* Set up vscode, python, github on other laptop for Braxton
* Download Tracking data and put in folder
* Figure out those neat tricks for making coding easier
    * Black for reformatting
    * Auto doc string
    * Hydra/MlFlow
## Coding
#### Prep
* Cleaning
    * Filter down types of plays, frames of interest, etc
    * Filter based on future feature engineering (Stunts)
* Feature Engineering
    * Stunts, etc
* Validation Viz
    * Set up field and annimation for play_id
#### Modeling
* Modeling
    * Cluster types of plays together --
    * Make a predicted path model for each play and player
    * Create Player Impact Distributions
    * Compute Differences between players and expected paths
    * Computer mathematically the impact of that with multiplication and integration to get number
        * Consider doing a weighting factor during the course of the play
    * Average over the length of the play for each player to account for play length
* Checking work
    * See if clustering plays works well or is worthwhile at all
    * Look into what RouteNet did for tracking route patterns and see if we are getting something similar --
    * Error rates for our predicted path model
    * Plot the Gravity over the course of the play to see if averaging is actually worth it
    * Eye test for highest gravity players
    * See if having two players with high gravity has higher impact on efficiency compared to Batman and Robin
#### Results
* Results
    * Create a table of the highest gravity players
    * Create a table of the team with highest average gravity
    * Create a table of the best Robins (High value players with low gravity playing next to someone with high gravity)
* Final Viz
    * Example Plays: 
        * Highlight Specific Player
        * Show Expected Path for relevant players
        * Show the orientation for players
        * Show a distribution getting darker and bigger as gravity increases
        * Show a line plot that shows the total gravity increasing as well as the players that are adding to the gravity over the course of the play
    * Explanation graphics that incorporate many intermediate steps and some math. Look at 2023 Winner for examples
## Submission
* Write up the submission in under 2000 words and 10 pictures
* Use the format that was most consistent across winners
* Be academic and available to use for research paper quality with appendix and figures
