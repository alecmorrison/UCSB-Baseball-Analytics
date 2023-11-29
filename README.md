# UCSB Baseball Analytics

## Swing Decision Metric

This repo contains various versions of my swing decision metric project, but the dataset is too large to push to github. This project compares every pitch seen by a UCSB batter to every previous pitch with similar location, speed, type. I create a rating for that pitch based on the number of expected bases on those similar pitches and use that to evalluate whether the batter made a good decision to swing or not. THe next steps for this project would be optimizing to store pitch ratings or create criteria to rate for faster data processing and using an algorithm to tailor preferred pitches for specific batters (requires more data). In order to see a result with more data, one script uses Statcast MLB data Scraped with python using pybaseball

## ML Project

This project uses a ridge regression to predict WAR for various players in the MLB.
