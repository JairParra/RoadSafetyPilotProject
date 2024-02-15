# Feature Engineering 

## Polynomial Expansions

### 1. **Traffic Flow Variables (`pi`, `fi`)**

- **Rationale**: Traffic flow variables, including pedestrian (`pi`) and vehicle flow (`fi`), are likely to have non-linear effects on accident rates. For example, a low to moderate increase in vehicle or pedestrian flow might not significantly impact accident rates, but beyond a certain threshold, the risk could increase exponentially due to higher interaction chances and potential congestion.
- **Proposal**: Include squared (and possibly cubic) terms for `pi` and `fi` to model these non-linear effects.

### 2. **Distance from Downtown (`distdt`)**

- **Rationale**: The effect of distance from downtown on accident rates is unlikely to be linear. Closer to downtown, where pedestrian and vehicle flows are higher, one might expect more accidents. However, the relationship could plateau or even decrease at some distance due to lower traffic density.
- **Proposal**: Include squared and cubic terms for `distdt` to capture potential non-linear decreases or increases in accident rates with distance.

### 3. **Crosswalk and Road Widths (`tot_crossw`, `avg_crossw`, `tot_road_w`)**

- **Rationale**: The dimensions of road and crosswalk features could have complex relationships with pedestrian safety. Wider roads might initially seem more dangerous due to longer crossing times, but very wide roads might also correspond to better-designed crossings with more safety features.
- **Proposal**: Consider squared terms for `tot_crossw`, `avg_crossw`, and `tot_road_w` to explore potential non-linear relationships.

### 4. **Turning Flows (`fli`, `fri`, `fti`)**

- **Rationale**: The flow of vehicles turning left, right, or going straight at intersections might impact pedestrian safety in non-linear ways, especially as interactions between turning vehicles and pedestrians are more unpredictable and potentially more hazardous.
- **Proposal**: Squared terms for `fli`, `fri`, and `fti` to assess if higher turning flows disproportionately increase accident risks.
