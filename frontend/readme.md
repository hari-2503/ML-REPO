# 🚴 Cycling Load Predictor

This is a full-stack machine learning app that helps cyclists track their weekly training load and prevent overtraining. It predicts how many more kilometers you can safely ride based on distance, pace, and elevation gain.

## 📦 Tech Stack

- **Frontend**: React + Chart.js + Axios
- **Backend**: Flask (Python)
- **Model**: RandomForestRegressor
- **Visualization**: Line graph + widgets

## 🚀 Getting Started

### Backend (Flask)
```bash
cd backend
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python app.py
