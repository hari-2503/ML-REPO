import React from 'react';
import { Line } from 'react-chartjs-2';
import {
  Chart as ChartJS,
  LineElement,
  CategoryScale,
  LinearScale,
  PointElement
} from 'chart.js';

ChartJS.register(LineElement, CategoryScale, LinearScale, PointElement);

export default function WeeklyChart({ data }) {
  const chartData = {
    labels: data.map(d => d.week),
    datasets: [
      {
        label: 'Weekly KM',
        data: data.map(d => d.distance),
        borderColor: 'blue',
        fill: false
      }
    ]
  };

  return <Line data={chartData} />;
}
