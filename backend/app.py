from flask import Flask, request, jsonify
from flask_cors import CORS
import joblib

app = Flask(__name__)
CORS(app)

# Load trained model
model = joblib.load('model.pkl')

@app.route('/predict', methods=['POST'])
def predict():
    data = request.json
    km = data.get('distance')
    pace = data.get('pace')
    elev = data.get('elevation')
    
    pred = model.predict([[km, elev, pace]])[0]
    extra_km = max(0, pred - km)

    return jsonify({
        "predicted_next_distance": round(pred, 1),
        "additional_km_allowed": round(extra_km, 1)
    })

if __name__ == '__main__':
    app.run(debug=True)
