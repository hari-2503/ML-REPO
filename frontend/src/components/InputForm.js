import React, { useState } from 'react';
import axios from 'axios';

export default function InputForm({ onResult }) {
  const [data, setData] = useState({ distance: '', elevation: '', pace: '' });

  const handleSubmit = async (e) => {
    e.preventDefault();
    const res = await axios.post('http://localhost:5000/predict', {
      ...data,
      distance: parseFloat(data.distance),
      elevation: parseFloat(data.elevation),
      pace: parseFloat(data.pace)
    });
    onResult(res.data);
  };

  return (
    <form onSubmit={handleSubmit}>
      <input placeholder="Distance (km)" onChange={(e) => setData({ ...data, distance: e.target.value })} />
      <input placeholder="Elevation (m)" onChange={(e) => setData({ ...data, elevation: e.target.value })} />
      <input placeholder="Avg Pace (min/km)" onChange={(e) => setData({ ...data, pace: e.target.value })} />
      <button type="submit">Submit</button>
    </form>
  );
}
