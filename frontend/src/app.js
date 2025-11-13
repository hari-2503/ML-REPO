import React, { useState } from 'react';
import InputForm from './components/InputForm';
import WeeklyChart from './components/WeeklyChart';
import StressWidget from './components/StressWidget';
import BalanceWidget from './components/BalanceWidget';

function App() {
  const [result, setResult] = useState(null);
  const [weeklyData, setWeeklyData] = useState([
    { week: 'Week 1', distance: 110 },
    { week: 'Week 2', distance: 125 },
    { week: 'Week 3', distance: 135 },
    { week: 'Week 4', distance: 120 }
  ]);

  return (
    <div style={{ padding: '20px' }}>
      <h2>Cycling Load Predictor</h2>
      <WeeklyChart data={weeklyData} />
      <InputForm onResult={setResult} />
      {result && (
        <>
          <StressWidget score={result.predicted_next_distance} />
          <BalanceWidget used={result.predicted_next_distance - result.additional_km_allowed} limit={result.predicted_next_distance} />
        </>
      )}
    </div>
  );
}

export default App;
