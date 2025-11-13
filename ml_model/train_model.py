import pandas as pd
from sklearn.ensemble import RandomForestRegressor
import joblib

# Load and clean data
df = pd.read_csv('../data/activities_raw.csv')
df = df[['Activity Date', 'Distance', 'Elapsed Time', 'Elevation Gain']].dropna()
df['Activity Date'] = pd.to_datetime(df['Activity Date'])
df['Week'] = df['Activity Date'].dt.to_period('W').apply(lambda r: r.start_time)

# Aggregate by week
weekly = df.groupby('Week').agg({
    'Distance': 'sum',
    'Elevation Gain': 'sum',
    'Elapsed Time': 'sum'
}).reset_index()
weekly['Avg_Pace'] = weekly['Elapsed Time'] / weekly['Distance'] / 60
weekly['Next_Distance'] = weekly['Distance'].shift(-1)
weekly.dropna(inplace=True)

X = weekly[['Distance', 'Elevation Gain', 'Avg_Pace']]
y = weekly['Next_Distance']

# Train model
model = RandomForestRegressor(n_estimators=100, random_state=42)
model.fit(X, y)

# Save model
joblib.dump(model, '../backend/model.pkl')
