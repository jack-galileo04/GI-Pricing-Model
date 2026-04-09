# GI Pricing Model

### Tools: R, tidyverse, generalised linear regression (GLM).
### Problem: Price motor insurance for a company to achieve a target loss ratio.

## Overview
This project builds a GLM model to price motor insurance using:
- Synthetic portfolio of policyholders
- Rating factors

## Data
Data is synthetically simulated.

## Results
- Simple GLMs of claim frequency and severity, to model pure premium.
- Pure premiums are then scaled to hit the target loss ratio.
- Segment analysis is then performed on the premiums.
- A well-fitted final premium model, with low bias to different risk profile segments.
