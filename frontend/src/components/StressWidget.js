import React from 'react';

export default function StressWidget({ score }) {
  return (
    <div style={{ marginTop: 20 }}>
      <h4>Predicted Stress Score</h4>
      <p>Next Week Estimated Load: <strong>{score.toFixed(1)} km</strong></p>
    </div>
  );
}
