import React from 'react';

export default function BalanceWidget({ used, limit }) {
  return (
    <div style={{ marginTop: 20 }}>
      <h4>Training Balance</h4>
      <p>{used.toFixed(1)} km ridden out of <strong>{limit.toFixed(1)} km</strong> predicted capacity.</p>
    </div>
  );
}
